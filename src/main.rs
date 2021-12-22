#![warn(missing_docs)]

//! Program to produce modulefile lines by looking at Linux shell environment differences
//!
//! # Call options
//! ```text
//! USAGE:
//! pmodenv [OPTIONS]
//!
//! FLAGS:
//!     -h, --help       Prints help information
//!     -V, --version    Prints version information
//!
//! OPTIONS:
//!     -c, --comment <COMMENT>       visible in the result
//!     -d, --drop <PATH>...          drop PATH
//!     -e, --except <VAR>...         ignores the environment variable VAR
//!     -p, --prefix <PREFIX>         turns PREFIX into a variable
//!     -r, --replace <OLD:NEW>...    replaces OLD path with NEW path
//!     -v, --var <VAR=VAL>...        turns VAL into a variable
//! ```
//!
//! # Example
//! ```text
//! $ cat /proc/self/environ > /tmp/env.txt
//! $ PATH=/opt/nvidia/bin:${HOME}/bin:${PATH} HELLO=1 ../target/debug/pmodenv -p ${HOME} -d nvidia -r bin:local/bin -r app:ap -v BIN=bin -e _ < /tmp/env.txt
//! ## produced by: ../target/debug/pmodenv -p /home/stadler_h -d nvidia -r bin:local/bin -r app:ap -v BIN=bin -e _
//! set PREFIX /home/stadler_h
//! set BIN bin
//! setenv HELLO 1
//! prepend-path PATH ${PREFIX}/local/${BIN}
//! ```

extern crate clap;
use std::collections::BTreeMap;

/// Name of the prefix variable
///
/// The path represented by `PREFIX_VAR` can be set using the `--prefix=<path>` commandline option
const PREFIX_VAR: &str = "PREFIX";

/// Replacement struct tuple
///
/// The first component is the replaced string, the second the replacement string
#[derive(Clone, Debug)]
struct Replacement<'a>(&'a str, &'a str);

/// Parse Linux shell environment from `io::stdin`
///
/// The input read from `io::stdin` is expected to be in the format of `/proc/self/environ`
///
/// # Argument
/// * `before` Map that will be filled with the environment variable to value mappings by reading `io::stdin`
/// # Error Handling
/// Crash if `io::stdin` doesn't deliver an environment in the format of `/proc/self/environ`
fn parse_env(before: &mut BTreeMap<String, String>)
{
    use std::io;
    use std::io::Read;

    let mut env_string = String::new();
    io::stdin().read_to_string(&mut env_string).ok().unwrap();
    for part in env_string.split('\0') {
        if part.contains('=') {
            let mut var_val = part.splitn(2, '=');
            let var = var_val.next().unwrap();
            let val = var_val.next().unwrap();
            before.insert(var.to_string(), val.to_string());
        }
    }
}

/// Return canonic path
///
/// # Argument
/// * `path` String slice representing a file system path
/// # Returns
/// String containing the canonicalized path. If `path` cannot be canonicalized, it is returned as is.
fn to_canonic(path: &str) -> String
{
    use std::fs::canonicalize;
    if let Ok(p) = canonicalize(path) {
        p.to_str().unwrap().to_string()
    } else {
        path.to_string()
    }
}

/// Map path components
///
/// The path is assumed to consist of components separated by `:`. Components are first canonicalized using [to_canonic], then the components matching drop fragments are dropped,
/// then fragment replacements are applied, and finally variable values are replaced with `${`VAR`}` and prefix is replaced by the `${`[PREFIX_VAR]`}`
///
/// # Argument
/// * `path` String slice representing a list of file system paths separated by `:`
/// * `prefix` String that will be replaced by `${`[PREFIX_VAR]`}`, typically a path prefix
/// * `drops` Optional vector of path fragments. Paths containing these will be dropped
/// * `replacements` Optional vector of path fragment replacements
/// * `vars` Optional variable to path fragment mapping
/// # Returns
/// String containing the mapped path.
fn map_path(path: &str,
            prefix: &Option<&str>,
            drops: &Option<Vec<&str>>,
            replacements: &Option<Vec<Replacement>>,
            vars: &Option<BTreeMap<&str, &str>>) -> String
{
    let mut path_list: Vec<String> = path.trim().trim_matches(':').split(':').map(to_canonic).collect();
    if let Some(drop_list) = drops {
        path_list = path_list.into_iter().filter(|p| !drop_list.iter().any(|d| p.contains(d))).collect();
    }
    if let Some(replacement_list) = replacements {
        path_list = path_list.into_iter().map(|p| replacement_list.iter().fold(p, |path, r| path.replace(r.0, r.1))).collect();
    }
    if let Some(vars_map) = vars {
        path_list = path_list.into_iter().map(|mut p| {
            for (var, val) in vars_map.iter() {
                p = p.replace(val, (String::from("${") + var + "}").as_str());
            }
            p
        }).collect();
    }
    if let Some(prf) = prefix {
        let prf = &to_canonic(prf);
        let pvar = "${".to_owned() + PREFIX_VAR + "}";
        path_list = path_list.into_iter().map(|p| p.replace(prf, &pvar)).collect();
    }
    path_list.into_iter().fold(String::from(""), |path, p| path + ":" + &p).trim_start_matches(':').to_string()
}

/// Parse commandline arguments
/// # Return
/// [clap::ArgMatches] object
fn parse_cli_args<'a>() -> clap::ArgMatches<'a>
{
    // NOTE: no positional arguments should be defined, as it leads to problems with options accepting multiple values, see clap docu
    use clap::{Arg, App};
    App::new("pmodenv")
        .version("1.0")
        .author("hstadler@protonmail.ch")
        .about("Turns environment differences into a module file

$ cat /proc/self/environ > /tmp/env.txt
$ PATH=${HOME}/bin:${PATH} HELLO=1 ../target/debug/pmodenv -p ${HOME} -e _ < /tmp/env.txt
# produced by: ../target/debug/pmodenv -p /home/stadler_h -e _
set PREFIX /home/stadler_h
setenv HELLO 1
prepend-path PATH ${PREFIX}/bin")
        .arg(Arg::with_name("except")
                .takes_value(true)
                .multiple(true)
                .short("e")
                .long("except")
                .value_name("VAR")
                .help("ignores the environment variable VAR"))
        .arg(Arg::with_name("replace")
                .takes_value(true)
                .multiple(true)
                .short("r")
                .long("replace")
                .value_name("OLD:NEW")
                .help("replaces OLD path with NEW path"))
        .arg(Arg::with_name("drop")
                .takes_value(true)
                .multiple(true)
                .short("d")
                .long("drop")
                .value_name("PATH")
                .help("drop PATH"))
        .arg(Arg::with_name("prefix")
                .takes_value(true)
                .short("p")
                .long("prefix")
                .value_name("PREFIX")
                .help("turns PREFIX into a variable"))
        .arg(Arg::with_name("var")
                .takes_value(true)
                .multiple(true)
                .short("v")
                .long("var")
                .value_name("VAR=VAL")
                .help("turns VAL into a variable"))
        .arg(Arg::with_name("comment")
                .takes_value(true)
                .short("c")
                .long("comment")
                .value_name("COMMENT")
                .help("visible in the result"))
        .get_matches()
}

/// Parse replacement arguments
///
/// This creates a vector of [Replacement] structures.
/// The replacement option string `old:new` will result in the vector element `Replacement("old", "new")`.
///
/// # Argument
/// * `replacements` Optional iterator over replacement option strings
/// # Returns
/// Optional vector of [Replacement] structures
fn parse_replacements<'a, T: Iterator<Item = &'a str>>(replacements: Option<T>) -> Option<Vec<Replacement<'a>>>
{
    if let Some(replacements_iter) = replacements {
        let replacements_list: Vec<Replacement<'a>> = replacements_iter.map(|s| {
                let mut split = s.splitn(2, ':');
                let orig: &'a str = split.next().unwrap_or_else(|| panic!("replace error: first part of {}", s));
                let repl: &'a str = split.next().unwrap_or_else(|| panic!("replace error: second part of {}", s));
                Replacement(orig, repl)
            }).collect();
        Some(replacements_list)
    } else {
        None
    }
}

/// Parse variable substitutions
///
/// Create a variable name to variable value mapping.
/// The variable subtitution string `var=val` will result in the mapping `"var"→"val"`
///
/// # Argument
/// * `vars` Optional iterator over variable substitution strings
/// # Return
/// Optional mapping from variable names to variable values
fn parse_variables<'a, T: Iterator<Item = &'a str>>(vars: Option<T>) -> Option<BTreeMap<&'a str, &'a str>>
{
    if let Some(vars_iter) = vars {
        let mut res = BTreeMap::<&str, &str>::new();
        vars_iter.fold(&mut res, |btree, v| {
            let mut split = v.splitn(2, '=');
            let var: &'a str = split.next().unwrap_or_else(|| panic!("var error: first part of {}", v));
            let val: &'a str = split.next().unwrap_or_else(|| panic!("var error: second part of {}", v));
            if let Some(dup) = btree.get(var) {
                panic!("duplicate definition of variable {}", dup);
            }
            btree.insert(var, val);
            btree
        });
        Some(res)
    } else {
        None
    }
}

/// Parse path drop fragments
///
/// Create a vector of path drop fragments.
///
/// # Argument
/// * `drops` Optional iterator over path drop fragments
/// # Return
/// Optional vector of path drop fragments
fn parse_drops<'a, T: Iterator<Item = &'a str>>(drops: Option<T>) -> Option<Vec<&'a str>>
{
    if let Some(drops_iter) = drops {
        Some(drops_iter.collect())
    } else {
        None
    }
}

/// Print module file header
///
/// The header will specify how the output was produced and define the variables
/// # Example
/// ```text
/// ## produced by: ../target/debug/pmodenv -p /home/stadler_h -v BIN=bin
/// set PREFIX /home/stadler_h
/// set BIN bin
/// ```
fn print_header(prefix: &Option<&str>, vars: &Option<BTreeMap<&str, &str>>)
{
    use std::env::args;
    print!("# produced by:");
    for arg in args() {
        print!(" {}", arg);
    }
    println!();
    if let Some(path) = prefix {
        println!("set {} {}", PREFIX_VAR, path);
    }
    if let Some(btree) = vars {
        for (var, val) in btree.iter() {
            println!("set {} {}", var, val);
        }
    }
}

/// Run the program
fn main()
{
    use std::env::vars;
    use std::collections::BTreeSet;

    let cli_args = parse_cli_args();

    let exceptions = cli_args.values_of("except");
    let replacements = parse_replacements(cli_args.values_of("replace"));
    let variables = parse_variables(cli_args.values_of("var"));
    let drops = parse_drops(cli_args.values_of("drop"));
    let prefix = cli_args.value_of("prefix");
    print_header(&prefix, &variables);

    let mut before = BTreeMap::new();
    parse_env(&mut before);
    let mut after = BTreeMap::new();
    for (var, val) in vars() {
        after.insert(var, val);
    }
    let vars_before : BTreeSet<&str> = before.keys().map(|s| s.as_str()).collect::<BTreeSet<&str>>();
    let vars_after : BTreeSet<&str> = after.keys().map(|s| s.as_str()).collect::<BTreeSet<&str>>();
    let mut vars : BTreeSet<&str> = vars_before.union(&vars_after).copied().collect::<BTreeSet<&str>>();
    if let Some(list) = exceptions {
        for var in list {
            vars.remove(var);
        }
    }
    for var in vars {
        if vars_before.contains(&var) {
            if vars_after.contains(&var) {
                let val_before = before.get(var).unwrap().trim();
                let val_after = after.get(var).unwrap().trim();
                if val_before != val_after {
                    if val_after.starts_with(val_before) {
                        let delta = val_after.get(val_before.len() .. val_after.len()).unwrap();
                        if delta.starts_with(':') {
                            println!("append-path {} {}", var, map_path(delta, &prefix, &drops, &replacements, &variables));
                        } else {
                            eprintln!("# WARNING: unsupported path suffix in variable: {}", var);
                        }
                    } else if val_after.ends_with(val_before) {
                        let delta = val_after.get(0 .. val_after.len() - val_before.len()).unwrap();
                        if delta.ends_with(':') {
                            println!("prepend-path {} {}", var, map_path(delta, &prefix, &drops, &replacements, &variables));
                        } else {
                            eprintln!("# WARNING: unsupported path prefix in variable: {}", var);
                        }
                    } else {
                        eprintln!("# WARNING: ignoring unsupported change in variable: {}", var);
                    }
                }
            } else {
                println!("unsetenv {}", var);
            }
        } else if var.to_lowercase().contains("path") {
            println!("prepend-path {} {}", var, map_path(after.get(var).unwrap(), &prefix, &drops, &replacements, &variables));
        } else {
            println!("setenv {} {}", var, after.get(var).unwrap());
        }
    }
}
