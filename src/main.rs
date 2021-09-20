#![warn(missing_docs)]

//! Program to produce modulefile lines by looking at Linux shell environment differences
//!
//! # Call options
//! ```text
//! USAGE:
//!    pmodenv [OPTIONS]
//!
//! FLAGS:
//!    -h, --help       Prints help information
//!    -V, --version    Prints version information
//!
//! OPTIONS:
//!    -c, --comment <COMMENT>       visible in the result
//!    -e, --except <VAR>...         ignores the environment variable VAR
//!    -p, --prefix <PREFIX>         turns PREFIX into a variable
//!    -r, --replace <OLD:NEW>...    replaces OLD path with NEW path
//! ```
//!
//! # Example
//! ```text
//! $ cat /proc/self/environ > /tmp/env.txt
//! $ PATH=${HOME}/bin:${PATH} HELLO=1 pmodenv -p ${HOME} -e _ < /tmp/env.txt
//! ## produced by: pmodenv -p /home/stadler_h -e _
//! set PREFIX /home/stadler_h
//! setenv HELLO 1
//! prepend-path PATH ${PREFIX}/bin
//! ```

extern crate clap;
use std::env::args;
use std::collections::BTreeMap;

/// Name of the prefix variable
///
/// The path represented by `PREFIX_VAR` can be set using the `--prefix=<path>` commandline option
const PREFIX_VAR: &str = "PREFIX";

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
/// The path is assumed to consist of components separated by `:`. Components are first canonicalized using [to_canonic], then the prefix is replaced by the `${`[PREFIX_VAR]`}`
///
/// # Argument
/// * `path` String slice representing a list of file system paths separated by `:`
/// * `prefix` String that will be replaced by `${`[PREFIX_VAR]`}`, typically a path prefix
/// # Returns
/// String containing the mapped path.
fn map_path(path: &str, prefix: Option<&str>) -> String
{
    let mut path_list = path.trim().trim_matches(':').split(':');
    if let Some(prf) = prefix {
        let prf = &to_canonic(prf);
        let pvar = "${".to_owned() + PREFIX_VAR + "}";
        let mut result = to_canonic(path_list.next().unwrap()).replace(prf, &pvar);
        for s in path_list {
            result = result + ":" + &(to_canonic(s).replace(prf, &pvar));
        }
        result
    } else {
        let mut result = to_canonic(path_list.next().unwrap());
        for s in path_list {
            result = result + ":" + &to_canonic(s);
        }
        result
    }
}

/// Parse commandline arguments
/// # Return
/// [clap::ArgMatches] object
fn parse_cli_args<'a>() -> clap::ArgMatches<'a>
{
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
        .arg(Arg::with_name("prefix")
                .takes_value(true)
                .short("p")
                .long("prefix")
                .value_name("PREFIX")
                .help("turns PREFIX into a variable"))
        .arg(Arg::with_name("comment")
                .takes_value(true)
                .short("c")
                .long("comment")
                .value_name("COMMENT")
                .help("visible in the result"))
        .get_matches()
}

/// Print module file header
///
/// The header will specify how the output was produced
/// # Example
/// ```text
/// ## produced by: pmodenv -p /home/stadler_h -e _
/// ```
fn print_header()
{
    print!("# produced by:");
    for arg in args() {
        print!(" {}", arg);
    }
    println!();
}

/// Run the program
fn main()
{
    use std::env;
    use std::collections::BTreeSet;

    let cli_args = parse_cli_args();

    print_header();

    let prefix = cli_args.value_of("prefix");
    if let Some(path) = prefix {
        println!("set {} {}", PREFIX_VAR, path);
    }

    let mut before = BTreeMap::new();
    parse_env(&mut before);
    let mut after = BTreeMap::new();
    for (var, val) in env::vars() {
        after.insert(var, val);
    }
    let vars_before : BTreeSet<&str> = before.keys().map(|s| s.as_str()).collect::<BTreeSet<&str>>();
    let vars_after : BTreeSet<&str> = after.keys().map(|s| s.as_str()).collect::<BTreeSet<&str>>();
    let mut vars : BTreeSet<&str> = vars_before.union(&vars_after).copied().collect::<BTreeSet<&str>>();
    let exceptions = cli_args.values_of("except");
    if let Some(values) = exceptions {
        for ref v in values {
            vars.remove(v);
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
                            println!("append-path {} {}", var, map_path(delta, prefix));
                        } else {
                            eprintln!("# WARNING: unsupported path suffix in variable: {}", var);
                        }
                    } else if val_after.ends_with(val_before) {
                        let delta = val_after.get(0 .. val_after.len() - val_before.len()).unwrap();
                        if delta.ends_with(':') {
                            println!("prepend-path {} {}", var, map_path(delta, prefix));
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
            println!("prepend-path {} {}", var, after.get(var).unwrap());
        } else {
            println!("setenv {} {}", var, after.get(var).unwrap());
        }
    }
}
