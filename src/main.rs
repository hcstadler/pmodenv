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

extern crate ansi_term;
extern crate clap;
use std::collections::BTreeMap;
use std::collections::BTreeSet;

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

/// Check if variable is a path
///
/// # Argument
/// * `var` Variable name
/// * `typmap` Variable name to type map
/// # Return
/// true if the variable is in the typmap with a nonempty separator
fn is_path(var: &str, typmap: &BTreeMap<&str, Option<&str>>) -> bool
{
    typmap.contains_key(var) && typmap.get(var).is_some()
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
    use ansi_term::Colour::Cyan;
    canonicalize(path).map_or_else(|_| Ok(path.to_string()),                                    // cannot canonicalize
                                   |p| if let Some(s) = p.to_str() {
                                       Ok(s.to_string())                                        // canonicalized path
                                    } else {
                                        Err(format!("unsupported path: {}", Cyan.paint(path)))  // path to utf8 conversion fails
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
    use clap::{Arg, App, crate_version};
    App::new("pmodenv")
        .version(crate_version!())
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
        .arg(Arg::with_name("type")
                .takes_value(true)
                .multiple(true)
                .short("t")
                .long("type")
                .value_name("VAR:TYPE[:SEP]")
                .help("Handle VAR as TYPE (p: path, n: normal), for TYPE=p the path separator SEP (default ':') will be used"))
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
    use ansi_term::Colour::{Cyan,Yellow};
    if let Some(replacements_iter) = replacements {
        let mut replacements_list = Vec::new();
        for s in replacements_iter {
            let mut split = s.splitn(2, ':');
            let orig = if let Some(s) = split.next() {
                s
            } else {
                return Err(format!("replacement: first part of {}, use OLD:NEW", Cyan.paint(s)))
            };
            let repl = if let Some(s) = split.next() {
                s
            } else {
                return Err(format!("replacement: second part of {}, use OLD:NEW", Cyan.paint(s)))
            };
            if orig.is_empty() || repl.is_empty() {
                return Err(format!("unsupported replacement {}, use OLD:NEW", Yellow.paint(s)))
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
    use ansi_term::Colour::{Cyan,Yellow};
    let mut btree = BTreeMap::new();
    prefix.and_then(|value| btree.insert(PREFIX_VAR, value));
    if let Some(vars_iter) = vars {
        for v in vars_iter {
            let mut split = v.splitn(2, '=');
            let var = if let Some(s) = split.next() {
                s
            } else {
                return Err(format!("variable: first part of {}, use VAR=VALUE", Cyan.paint(v)))
            };
            let val = if let Some(s) = split.next() {
                s
            } else {
                return Err(format!("variable: second part of {}, use VAR=VALUE", Cyan.paint(v)))
            };
            if var.is_empty() || val.is_empty() {
                return Err(format!("variable substitution {}, use VAR=VALUE", Yellow.paint(v)))
            }
            if btree.get(var).is_some() {
                return Err(format!("duplicate definition of variable {}", Cyan.paint(var)))
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

/// Parse variable types
///
/// Create a variable name to variable type mapping.
/// The variable type string `var:t:s` will result in
/// * `"var"→None` for t='n'
/// * `"var"→Some("separator") for t='p'
///
/// # Argument
/// * `types` Optional iterator over variable type strings
/// # Return
/// Potentially empty mapping from variable names to variable types
fn parse_vartypes<'a, T: Iterator<Item = &'a str>>(types: Option<T>) -> Result<BTreeMap<&'a str, Option<&'a str>>, String>
{
    use ansi_term::Colour::{Cyan,Yellow};
    let mut btree = BTreeMap::new();
    if let Some(types_iter) = types {
        for v in types_iter {
            let mut split = v.splitn(3, ':');
            let var = if let Some(s) = split.next() {
                s
            } else {
                return Err(format!("type: first part of {}, use VAR:TYPE[:SEP]", Cyan.paint(v)))
            };
            let typ = if let Some(s) = split.next() {
                s
            } else {
                return Err(format!("type: second part of {}, use VAR:TYPE[:SEP]", Cyan.paint(v)))
            };
            let mut sep = split.next();
            if var.is_empty() || typ.is_empty() {
                return Err(format!("type {}, use VAR:TYPE[:SEP]", Yellow.paint(v)))
            }
            if typ == "p" {
                if let Some(s) = sep {
                    if s.is_empty() {
                        sep = Some(":")
                    }
                }
            } else if typ == "n" {
                if sep.is_some() {
                    return Err(format!("type: no separator allowed for {}:n", Cyan.paint(var)))
                }
            } else {
                return Err(format!("type: TYPE {} for variable {} undefined, use one of 'n', 'p'", Yellow.paint(typ), Cyan.paint(var)))
            }
            if btree.get(var).is_some() {
                return Err(format!("duplicate definition of type for variable {}", Cyan.paint(var)))
            }
            btree.insert(var, sep);
        }
    }
    Ok(btree)
}

/// Update type map with unmentioned path variables according to heuristic
///
/// If a lowercased variable name contains 'path', it is assumed to be a traditional path variable with separator ':'
///
/// # Argument
/// * `typmap` The variable to type map that will be updated
/// * `vars` Set of variables to consider
fn update_vartypes<'a>(typmap: &mut BTreeMap<&'a str, Option<&'a str>>, vars: &BTreeSet<&'a str>)
{
    for var in vars.iter() {
        if ! typmap.contains_key(var)
           && var.to_lowercase().contains("path") {
               typmap.insert(var, Some(":"));
        }
    }
}

/// Print one line of output
///
/// # Argument
/// * `s` String representing one output line
/// # Return
/// Error if argument *s* represents more than one line
fn print_str(s: &str) -> Result<(), String>
{
    use ansi_term::Colour::Yellow;
    if s.lines().count() > 1 {
        return Err(format!("cannot handle multiline output:\n{}", Yellow.paint(s)))
    }
    println!("{}", s);
    Ok(())
}

/// Unary operation on variable
///
/// # Argument
/// * `op` Unary operation name
/// * `var` Variable name
/// # Example
/// ```
/// unsetenv HELLO
/// ```
fn print_var(op: &str, var: &str) -> Result<(), String>
{
    print_str(&format!("{} {}", op, var))
}

/// Binary operation on variable
///
/// # Argument
/// * `op` Binary operation name
/// * `var` Variable name
/// * `val` Operand
/// # Example
/// ```
/// setenv HELLO 1
/// ```
fn print_var_val(op: &str, var: &str, val: &str) -> Result<(), String>
{
    if !val.is_empty() {
        print_str(&format!("{} {} {}", op, var, val))?
    }
    Ok(())
}

/// Print module file header
///
/// The header will specify how the output was produced and define the variables
///
/// # Argument
/// * `vars` Variable substitution map
/// # Example
/// ```text
/// ## produced by: ../target/debug/pmodenv -p /home/stadler_h -v BIN=bin
/// set PREFIX /home/stadler_h
/// set BIN bin
/// ```
fn print_header(vars: &Option<BTreeMap<&str, &str>>) -> Result<(), String>
{
    use std::env::args;
    let comment = args().fold(String::from("# produced by:"), |s, arg| s + " " + &arg);
    print_str(&comment)?;
    if let Some(btree) = vars.as_ref() {
        for (var, val) in btree.iter() {
            print_var_val("set", var, val)?
        }
    }
    Ok(())
}

/// Run the program
fn run() -> Result<(), String>
{
    use std::env::vars;
    use ansi_term::Colour::Cyan;

    let cli_args = parse_cli_args();

    let exceptions = cli_args.values_of("except");
    let replacements = parse_replacements(cli_args.values_of("replace"))?;
    let variables = parse_variables(cli_args.value_of("prefix"), cli_args.values_of("var"))?;
    let mut vartypes = parse_vartypes(cli_args.values_of("type"))?;
    let drops = parse_drops(cli_args.values_of("drop"));
    let check_path = cli_args.is_present("check-path");
    print_header(&variables)?;

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
    update_vartypes(&mut vartypes, &vars);

    for var in vars {
        if vars_before.contains(&var) {
            if vars_after.contains(&var) {
                let val_before = before.get(var).unwrap().trim();
                let val_after = after.get(var).unwrap().trim();
                if val_before != val_after {
                    if val_after.starts_with(val_before) {
                        let delta = val_after.get(val_before.len() .. val_after.len()).unwrap();
                        if delta.starts_with(':') ^ val_before.ends_with(':') {
                            print_var_val("append-path", var, &map_path(delta, drops.as_deref(), replacements.as_deref(), variables.as_ref(), check_path)?)?
                        } else {
                            eprintln!("# WARNING: unsupported path suffix in variable: {}", Cyan.paint(var))
                        }
                    } else if val_after.ends_with(val_before) {
                        let delta = val_after.get(0 .. val_after.len() - val_before.len()).unwrap();
                        if delta.ends_with(':') ^ val_before.starts_with(':') {
                            print_var_val("prepend-path", var, &map_path(delta, drops.as_deref(), replacements.as_deref(), variables.as_ref(), check_path)?)?
                        } else {
                            eprintln!("# WARNING: unsupported path prefix in variable: {}", Cyan.paint(var))
                        }
                    } else if val_after.contains(val_before) {
                        let start_end = val_after.split(val_before).collect::<Vec<&str>>();
                        if start_end.len() != 2 {
                            eprintln!("# WARNING: ignoring unexpected change in variable: {}", Cyan.paint(var))
                        }
                        print_var_val("prepend-path", var, &map_path(start_end[0], drops.as_deref(), replacements.as_deref(), variables.as_ref(), check_path)?)?;
                        print_var_val("append-path", var, &map_path(start_end[1], drops.as_deref(), replacements.as_deref(), variables.as_ref(), check_path)?)?;
                    } else {
                        eprintln!("# WARNING: ignoring unsupported change in variable: {}", Cyan.paint(var))
                    }
                }
            } else {
                print_var("unsetenv", var)?
            }
        } else {
            let path = map_path(after.get(var).unwrap(), drops.as_deref(), replacements.as_deref(), variables.as_ref(), check_path)?;
            if is_path(&var, &vartypes) {
                print_var_val("prepend-path", var, &path)?
            } else {
                print_var_val("setenv", var, &path)?
            }
        }
    }
    Ok(())
}

/// Run program and handle errors
fn main()
{
    use ansi_term::Colour::Red;

    if let Err(msg) = run() {
        eprintln!("{} {}", Red.paint("error:"), msg)
    }
}
