#![warn(missing_docs)]

//! Program to produce modulefile lines by looking at Linux shell environment differences
//!
//! # Call options
//! ```text
//! USAGE:
//!     pmodenv [FLAGS] [OPTIONS]
//!
//! FLAGS:
//!     -x, --check-path    Only keep path components that are accessible by current user on the local machine
//!     -h, --help          Prints help information
//!     -V, --version       Prints version information
//!
//! OPTIONS:
//!     -c, --comment <COMMENT>       Visible in the result
//!     -d, --drop <FRAGMENT>...      Drop paths containing FRAGMENT
//!     -e, --except <VAR>...         Ignores the environment variable VAR
//!     -p, --prefix <PREFIX>         Turns PREFIX into a variable
//!     -r, --replace <OLD:NEW>...    Replaces OLD path fragment with NEW path fragment
//!     -s, --var <VAR=VAL>...        Turns VAL into a variable
//! USAGE:
//! ```
//!
//! # Example
//! ```text
//! $ cat /proc/self/environ > /tmp/env.txt
//! $ PATH=/opt/nvidia/bin:${HOME}/bin:${PATH} HELLO=1 ../target/debug/pmodenv -p ${HOME} -s NVIDIA_HOME=/opt/nvidia -e _ < /tmp/env.txt
//! ## produced by: ../target/debug/pmodenv -p /home/stadler_h -s NVIDIA_HOME=/opt/nvidia -e _
//! set NVIDIA_HOME /opt/nvidia
//! set PREFIX /home/stadler_h
//! setenv HELLO 1
//! prepend-path PATH ${NVIDIA_HOME}/bin:${PREFIX}/bin
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
fn to_canonic(path: &str) -> Result<String, String>
{
    use std::fs::canonicalize;
    canonicalize(path).map_or_else(|_| Ok(path.to_string()),
                                   |p| if let Some(s) = p.to_str() {
                                       Ok(s.to_string())
                                    } else {
                                        Err(format!("unsupported path: {}", path))
                                    })
}

/// Map path components
///
/// The path is assumed to consist of components separated by `:`.
/// Components are first canonicalized using [to_canonic],
/// then nonexistent components and those matching drop fragments are dropped,
/// then fragment replacements are applied (in slice element order),
/// and finally variable values (in cluding prefix) are replaced in random order with `${`VAR`}` (or `${`[PREFIX_VAR]`}`, resp)
///
/// # Argument
/// * `path` String slice representing a list of file system paths separated by `:`
/// * `drops` Optional vector of path fragments. Paths containing these will be dropped
/// * `replacements` Optional vector of path fragment replacements
/// * `vars` Optional variable to path fragment mapping, including prefix
/// * `check_path` If true, filter out paths that are not accessible
/// # Returns
/// String containing the mapped path.
fn map_path(path: &str,
            drops: Option<&[&str]>,
            replacements: Option<&[Replacement]>,
            vars: Option<&BTreeMap<&str, &str>>,
            check_path: bool) -> Result<String, String>
{
    use std::path::Path;
    let mut path_list = Vec::new();
    for p in path.trim().split(':').filter(|p| !p.is_empty()) {
        path_list.push(to_canonic(p)?)
    }
    if check_path {
        path_list = path_list.into_iter().filter(|p| Path::new(p).exists()).collect()
    }
    if let Some(drop_list) = drops {
        path_list = path_list.into_iter().filter(|p| !drop_list.iter().any(|d| p.contains(d))).collect()
    }
    if let Some(replacement_list) = replacements {
        path_list = path_list.into_iter().map(|p| replacement_list.iter().fold(p, |path, r| path.replace(r.0, r.1))).collect()
    }
    if let Some(vars_map) = vars {
        path_list = path_list.into_iter().map(|mut p| {
            vars_map.iter().for_each(|(var, val)| p = p.replace(val, &(String::from("${") + var + "}")));
            p
        }).collect()
    }
    Ok(path_list.into_iter().fold(String::from(""), |path, p| path + ":" + &p).trim_start_matches(':').to_string())
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

EXAMPLE:
  $ cat /proc/self/environ > /tmp/env.txt
  $ PATH=${HOME}/bin:${PATH} HELLO=1 ../target/debug/pmodenv -p ${HOME} -e _ < /tmp/env.txt
  # produced by: ../target/debug/pmodenv -p /home/stadler_h -e _
  set PREFIX /home/stadler_h
  setenv HELLO 1
  prepend-path PATH ${PREFIX}/bin

PRECONDITIONS:
  - Variable names are assumed to be 'nice': no strange characters.
  - Path environment variables are assumed to have PATH in their name.
  - Variable values are assumed to be 'nice' as well.
  - Paths are assumed to be 'nicely' separated by ':'.
  - Replacement and variable options are assumed to be 'nice': no nonsense overlaps

EXECUTION:
  For path changes
  1 - Canonicalize components in the change
  2 - Drop components that cannot be accessed (optional)
  3 - Drop components containing drop option fragments (optional)
  4 - Apply fragment replacements to components in the order given on the commandline (optional)
  5 - Substitute variable value fragments of the components with the variables (including PREFIX)
      in random order (optional)

SECURITY:
  Never use the output without checking it, as some people are not 'nice'.")
        .arg(Arg::with_name("except")
                .takes_value(true)
                .multiple(true)
                .short("e")
                .long("except")
                .value_name("VAR")
                .help("Ignores the environment variable VAR"))
        .arg(Arg::with_name("replace")
                .takes_value(true)
                .multiple(true)
                .short("r")
                .long("replace")
                .value_name("OLD:NEW")
                .help("Replaces OLD path fragment with NEW path fragment"))
        .arg(Arg::with_name("drop")
                .takes_value(true)
                .multiple(true)
                .short("d")
                .long("drop")
                .value_name("FRAGMENT")
                .help("Drop paths containing FRAGMENT"))
        .arg(Arg::with_name("check-path")
                .takes_value(false)
                .short("x")
                .long("check-path")
                .help("Only keep path components that are accessible by current user on the local machine"))
        .arg(Arg::with_name("prefix")
                .takes_value(true)
                .short("p")
                .long("prefix")
                .value_name("PREFIX")
                .help("Turns PREFIX into a variable"))
        .arg(Arg::with_name("var")
                .takes_value(true)
                .multiple(true)
                .short("s")
                .long("var")
                .value_name("VAR=VAL")
                .help("Turns VAL into a variable"))
        .arg(Arg::with_name("comment")
                .takes_value(true)
                .short("c")
                .long("comment")
                .value_name("COMMENT")
                .help("Visible in the result"))
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
fn parse_replacements<'a, T: Iterator<Item = &'a str>>(replacements: Option<T>) -> Result<Option<Vec<Replacement<'a>>>, String>
{
    if let Some(replacements_iter) = replacements {
        let mut replacements_list = Vec::new();
        for s in replacements_iter {
            let mut split = s.splitn(2, ':');
            let orig = if let Some(s) = split.next() {
                s
            } else {
                return Err(format!("replace error: first part of {}", s))
            };
            let repl = if let Some(s) = split.next() {
                s
            } else {
                return Err(format!("replace error: second part of {}", s))
            };
            if orig.is_empty() || repl.is_empty() {
                return Err(format!("unsupported replacement for <{}>", orig))
            }
            replacements_list.push(Replacement(orig, repl));
        }
        Ok(Some(replacements_list))
    } else {
        Ok(None)
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
fn parse_variables<'a, T: Iterator<Item = &'a str>>(prefix: Option<&'a str>, vars: Option<T>) -> Result<Option<BTreeMap<&'a str, &'a str>>, String>
{
    let mut btree = BTreeMap::new();
    prefix.and_then(|value| btree.insert(PREFIX_VAR, value));
    if let Some(vars_iter) = vars {
        for v in vars_iter {
            let mut split = v.splitn(2, '=');
            let var = if let Some(s) = split.next() {
                s
            } else {
                return Err(format!("var error: first part of {}", v))
            };
            let val = if let Some(s) = split.next() {
                s
            } else {
                return Err(format!("var error: second part of {}", v))
            };
            if var.is_empty() || val.is_empty() {
                return Err(format!("unsupported variable substitution for <{}>", var))
            }
            if btree.get(var).is_some() {
                return Err(format!("duplicate definition of variable {}", var))
            }
            btree.insert(var, val);
        }
    }
    if btree.is_empty() {
        Ok(None)
    } else {
        Ok(Some(btree))
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
    drops.map(|drops_iter| drops_iter.collect())
}

fn print_str(s: &str)
{
    if s.lines().count() > 1 {
        panic!("cannot handle multiline output:\n{}", s)
    }
    println!("{}", s);
}

fn print_var(op: &str, var: &str)
{
    print_str(&format!("{} {}", op, var));
}

fn print_var_val(op: &str, var: &str, val: &str)
{
    if !val.is_empty() {
        print_str(&format!("{} {} {}", op, var, val))
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
fn print_header(vars: &Option<BTreeMap<&str, &str>>)
{
    use std::env::args;
    let comment = args().fold(String::from("# produced by:"), |s, arg| s + " " + &arg);
    print_str(&comment);
    if let Some(btree) = vars.as_ref() {
        btree.iter().for_each(|(var, val)| print_var_val("set", var, val))
    }
}

/// Run the program
fn run() -> Result<(), String>
{
    use std::env::vars;
    use std::collections::BTreeSet;

    let cli_args = parse_cli_args();

    let exceptions = cli_args.values_of("except");
    let replacements = parse_replacements(cli_args.values_of("replace"))?;
    let variables = parse_variables(cli_args.value_of("prefix"), cli_args.values_of("var"))?;
    let drops = parse_drops(cli_args.values_of("drop"));
    let check_path = cli_args.is_present("check-path");
    print_header(&variables);

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
        list.for_each(|var| { vars.remove(var); })
    };
    for var in vars {
        if vars_before.contains(&var) {
            if vars_after.contains(&var) {
                let val_before = before.get(var).unwrap().trim();
                let val_after = after.get(var).unwrap().trim();
                if val_before != val_after {
                    if val_after.starts_with(val_before) {
                        let delta = val_after.get(val_before.len() .. val_after.len()).unwrap();
                        if delta.starts_with(':') ^ val_before.ends_with(':') {
                            print_var_val("append-path", var, &map_path(delta, drops.as_deref(), replacements.as_deref(), variables.as_ref(), check_path)?)
                        } else {
                            eprintln!("# WARNING: unsupported path suffix in variable: {}", var)
                        }
                    } else if val_after.ends_with(val_before) {
                        let delta = val_after.get(0 .. val_after.len() - val_before.len()).unwrap();
                        if delta.ends_with(':') ^ val_before.starts_with(':') {
                            print_var_val("prepend-path", var, &map_path(delta, drops.as_deref(), replacements.as_deref(), variables.as_ref(), check_path)?)
                        } else {
                            eprintln!("# WARNING: unsupported path prefix in variable: {}", var)
                        }
                    } else if val_after.contains(val_before) {
                        let start_end = val_after.split(val_before).collect::<Vec<&str>>();
                        if start_end.len() != 2 {
                            eprintln!("# WARNING: ignoring unexpected change in variable: {}", var)
                        }
                        print_var_val("prepend-path", var, &map_path(start_end[0], drops.as_deref(), replacements.as_deref(), variables.as_ref(), check_path)?);
                        print_var_val("append-path", var, &map_path(start_end[1], drops.as_deref(), replacements.as_deref(), variables.as_ref(), check_path)?);
                    } else {
                        eprintln!("# WARNING: ignoring unsupported change in variable: {}", var)
                    }
                }
            } else {
                print_var("unsetenv", var)
            }
        } else {
            let path = map_path(after.get(var).unwrap(), drops.as_deref(), replacements.as_deref(), variables.as_ref(), check_path)?;
            if var.to_lowercase().contains("path") {
                print_var_val("prepend-path", var, &path)
            } else {
                print_var_val("setenv", var, &path)
            }
        }
    }
    Ok(())
}

/// Run program and handle errors
fn main()
{
    if let Err(msg) = run() {
        eprintln!("Error: {}", msg)
    }
}
