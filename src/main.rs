#![warn(missing_docs)]

//! Program to produce modulefile lines by looking at Linux shell environment differences.
//! Variables containing PATH, ROOT, HOME, EXE, PREFIX, FILE, or DIR are assumed to be path variables,
//! unless overridden by the --type option.
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
//!     -c, --comment <COMMENT>           Visible in the result
//!     -d, --drop <FRAGMENT>...          Drop paths containing FRAGMENT
//!     -e, --except <VAR>...             Ignores the environment variable VAR
//!     -p, --prefix <PREFIX>             Turns PREFIX into a variable
//!     -r, --replace <OLD:NEW>...        Replaces OLD path fragment with NEW path fragment
//!     -t, --type <VAR:TYPE[:SEP]>...    Handle VAR as TYPE (p: path, n: normal), for TYPE=p the path separator SEP
//!                                       (default ':') will be used
//!     -s, --var <VAR=VAL>...            Turns VAL into a variable
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
use clap::{crate_authors, crate_description, crate_name, crate_version, Parser};
use std::collections::BTreeMap;
use std::collections::BTreeSet;

extern crate text_colorizer;
use text_colorizer::Colorize;

/// Name of the prefix variable
///
/// The path represented by `PREFIX_VAR` can be set using the `--prefix=<path>` commandline option
const PREFIX_VAR: &str = "PREFIX";

/// Default path separator
const PATH_SEP: &str = ":";

/// Commandline arguments
#[derive(Parser, Debug)]
#[command(name = crate_name!(),
          author = crate_authors!(),
          version = crate_version!(),
          about = crate_description!(),
          long_about = None,
          after_help =
"HEURISTIC:
    Variables containing PATH, ROOT, HOME, EXE, PREFIX, FILE, or DIR are assumed to be path variables,
    unless overridden by the --type option.

EXAMPLE:
    $ cat /proc/self/environ > /tmp/env.txt
    $ PATH=${HOME}/bin:${PATH} HELLO=1 pmodenv -p ${HOME} -e _ < /tmp/env.txt
    # produced by: pmodenv -p /home/stadler_h -e _
    set PREFIX /home/stadler_h
    setenv HELLO 1
    prepend-path PATH ${PREFIX}/bin

PRECONDITIONS:
    - Variable names are assumed to be nice: no strange characters.
    - Path environment variables are assumed to have PATH in their name.
    - Variable values are assumed to be nice as well.
    - Paths are assumed to be nicely separated by ':' (or SEP).
    - Replacement and variable options are assumed to be nice: no nonsense overlaps

EXECUTION:
    For path changes
    1 - Canonicalize components in the change
    2 - Drop components that cannot be accessed (optional)
    3 - Drop components containing drop option fragments (optional)
    4 - Apply fragment replacements to components in the order given on the commandline (optional)
    5 - Substitute variable value fragments of the components with the variables (including PREFIX)
        in random order (optional)

SECURITY:
    Never use the output without checking it, as some people are not nice."
)]
struct Args {
    #[arg(
        short = 'e',
        long,
        value_name = "VAR",
        help = "Ignores the environment variable VAR"
    )]
    except: Vec<String>,
    #[arg(
        short = 'r',
        long,
        value_name = "OLD:NEW",
        help = "Replaces OLD path fragment with NEW path fragment"
    )]
    replace: Vec<String>,
    #[arg(
        short = 'd',
        long,
        value_name = "FRAGMENT",
        help = "Drop paths containing FRAGMENT"
    )]
    drop: Vec<String>,
    #[arg(
        short = 'x',
        long,
        help = "Only keep path components that are accessible by current user on the local machine"
    )]
    check_path: bool,
    #[arg(short = 'p', long, help = "Turns PREFIX into a variable")]
    prefix: Option<String>,
    #[arg(
        short = 's',
        long,
        value_name = "VAR=VAL",
        help = "Turns VAL into a variable"
    )]
    var: Vec<String>,
    #[arg(
        id = "type",
        short = 't',
        long,
        value_name = "VAR:TYPE[:SEP]",
        help = "Handle VAR as TYPE (p: path, n: normal), for TYPE=p the path separator SEP (default ':') will be used"
    )]
    typ: Vec<String>,
    #[arg(short = 'c', long, help = "Visible in the result")]
    comment: Option<String>,
}

/// Replacement struct tuple
///
/// The first component is the replaced string, the second the replacement string
#[derive(Clone, Debug)]
struct Replacement(String, String);

/// Transform arguments
///
/// Contains transformation arguments
/// * `drops` Drop paths with these fragments
/// * `replacements` Target and replacement path fragments
/// * `vars` Variable substitutions
/// * `check_path` Check for nonaccessible paths
#[derive(Clone, Debug)]
struct Transform<'a> {
    drops: &'a [String],
    replacements: &'a [Replacement],
    vars: &'a BTreeMap<String, String>,
    check_path: bool,
}

/// Parse Linux shell environment from `io::stdin`
///
/// The input read from `io::stdin` is expected to be in the format of `/proc/self/environ`
///
/// # Argument
/// * `before` Map that will be filled with the environment variable to value mappings by reading `io::stdin`
/// # Error Handling
/// Crash if `io::stdin` doesn't deliver an environment in the format of `/proc/self/environ`
fn parse_env(before: &mut BTreeMap<String, String>) {
    use std::io;
    use std::io::Read;

    let mut env_string = String::new();
    io::stdin().read_to_string(&mut env_string).ok().unwrap();
    for part in env_string.split('\0') {
        if part.contains('=') {
            let (var, val) = part.split_once('=').unwrap();
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
fn is_path(var: &str, typmap: &BTreeMap<String, Option<String>>) -> bool {
    typmap.contains_key(var) && typmap.get(var).unwrap().is_some()
}

/// Return canonic path
///
/// # Argument
/// * `path` String slice representing a file system path
/// # Returns
/// String containing the canonicalized path. If `path` cannot be canonicalized, it is returned as is.
fn to_canonic(path: &str) -> Result<String, String> {
    use std::fs::canonicalize;

    canonicalize(path).map_or_else(
        |_| Ok(path.to_string()), // cannot canonicalize
        |p| {
            if let Some(s) = p.to_str() {
                Ok(s.to_string()) // canonicalized path
            } else {
                Err(format!("unsupported path: {}", path.cyan())) // path to utf8 conversion fails
            }
        },
    )
}

/// Map path components
///
/// The path is assumed to consist of components separated by `sep`.
/// Components are first canonicalized using [to_canonic],
/// then nonexistent components and those matching drop fragments are dropped,
/// then fragment replacements are applied (in slice element order),
/// and finally variable values (in cluding prefix) are replaced in random order with `${`VAR`}` (or `${`[PREFIX_VAR]`}`, resp)
///
/// # Argument
/// * `path` String slice representing a list of file system paths separated by `sep`
/// * `sep` Path separator
/// * `transform` Transform arguments
/// # Returns
/// String containing the mapped path.
fn map_path(path: &str, sep: &str, transform: &Transform) -> Result<String, String> {
    use std::path::Path;

    let mut path_list = Vec::new();
    for p in path.trim().split(sep).filter(|p| !p.is_empty()) {
        path_list.push(to_canonic(p)?)
    }
    if transform.check_path {
        path_list.retain(|p| Path::new(p).exists())
    }
    if !transform.drops.is_empty() {
        path_list.retain(|p| !transform.drops.iter().any(|d| p.contains(d)))
    }
    if !transform.replacements.is_empty() {
        path_list = path_list
            .into_iter()
            .map(|p| {
                transform
                    .replacements
                    .iter()
                    .fold(p, |path, r| path.replace(&r.0, &r.1))
            })
            .collect()
    }
    if !transform.vars.is_empty() {
        path_list = path_list
            .into_iter()
            .map(|mut p| {
                transform
                    .vars
                    .iter()
                    .for_each(|(var, val)| p = p.replace(val, &(String::from("${") + var + "}")));
                p
            })
            .collect()
    }
    Ok(path_list
        .into_iter()
        .fold(String::from(""), |path, p| path + sep + &p)
        .trim_start_matches(sep)
        .to_string())
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
fn parse_replacements(replacements: Vec<String>) -> Result<Vec<Replacement>, String> {
    let mut replacements_list = Vec::new();
    if !replacements.is_empty() {
        for r in replacements {
            let mut split = r.splitn(2, ':');
            let orig = if let Some(s) = split.next() {
                if s.is_empty() {
                    return Err(format!(
                        "unsupported replacement {}, use OLD:NEW",
                        r.yellow()
                    ));
                };
                s
            } else {
                return Err(format!(
                    "replacement: first part of {}, use OLD:NEW",
                    r.cyan()
                ));
            };
            let repl = split.next().unwrap_or("");
            replacements_list.push(Replacement(orig.to_string(), repl.to_string()));
        }
    }
    Ok(replacements_list)
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
fn parse_variables(
    prefix: Option<String>,
    vars: Vec<String>,
) -> Result<BTreeMap<String, String>, String> {
    let mut btree = BTreeMap::new();
    prefix.and_then(|value| btree.insert(PREFIX_VAR.to_string(), value));
    if !vars.is_empty() {
        for v in vars {
            let mut split = v.splitn(2, '=');
            let var = if let Some(s) = split.next() {
                s
            } else {
                return Err(format!(
                    "variable: first part of {}, use VAR=VALUE",
                    v.cyan()
                ));
            };
            let val = if let Some(s) = split.next() {
                s
            } else {
                return Err(format!(
                    "variable: second part of {}, use VAR=VALUE",
                    v.cyan()
                ));
            };
            if var.is_empty() || val.is_empty() {
                return Err(format!(
                    "variable substitution {}, use VAR=VALUE",
                    v.yellow()
                ));
            }
            if btree.get(var).is_some() {
                return Err(format!("duplicate definition of variable {}", var.cyan()));
            }
            btree.insert(var.to_string(), val.to_string());
        }
    }
    Ok(btree)
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
fn parse_vartypes(types: Vec<String>) -> Result<BTreeMap<String, Option<String>>, String> {
    let mut btree = BTreeMap::new();
    if !types.is_empty() {
        for v in types {
            let mut split = v.splitn(3, ':');
            let var = if let Some(s) = split.next() {
                s
            } else {
                return Err(format!(
                    "type: first part of {}, use VAR:TYPE[:SEP]",
                    v.cyan()
                ));
            };
            let typ = if let Some(s) = split.next() {
                s
            } else {
                return Err(format!(
                    "type: second part of {}, use VAR:TYPE[:SEP]",
                    v.cyan()
                ));
            };
            let mut sep = split.next();
            if var.is_empty() || typ.is_empty() {
                return Err(format!("type {}, use VAR:TYPE[:SEP]", v.yellow()));
            }
            if typ == "p" {
                if let Some(s) = sep {
                    if s.is_empty() {
                        sep = Some(PATH_SEP)
                    }
                } else {
                    sep = Some(PATH_SEP)
                }
            } else if typ == "n" {
                if sep.is_some() {
                    return Err(format!("type: no separator allowed for {}:n", var.cyan()));
                }
            } else {
                return Err(format!(
                    "type: TYPE {} for variable {} undefined, use one of 'n', 'p'",
                    typ.yellow(),
                    var.cyan()
                ));
            }
            if btree.get(var).is_some() {
                return Err(format!(
                    "duplicate definition of type for variable {}",
                    var.cyan()
                ));
            }
            btree.insert(var.to_string(), sep.map(|s| s.to_string()));
        }
    }
    Ok(btree)
}

/// Update type map with unmentioned path variables according to heuristic
///
/// If a lowercased variable name contains 'path', 'home', 'root', 'file', 'exe', 'prefix', 'dir',
/// it is assumed to be a traditional path variable with separator ':'
///
/// # Argument
/// * `typmap` The variable to type map that will be updated
/// * `vars` Set of variables to consider
fn update_vartypes(typmap: &mut BTreeMap<String, Option<String>>, vars: &BTreeSet<&str>) {
    for var in vars.iter() {
        if !typmap.contains_key(&var.to_string()) {
            let var_to_lower = var.to_lowercase();
            for part in ["path", "home", "root", "file", "exe", "prefix", "dir"] {
                if var_to_lower.contains(part) {
                    typmap.insert(var.to_string(), Some(PATH_SEP.to_string()));
                    break;
                }
            }
        }
    }
}

/// Convert string to TCL friendly string
///
/// If the string contains whitespace, wrap it in "
/// ***WARNING***: This is very incomplete, TCL has many special chars it interprets!
///
/// # Argument
/// * `s` String
/// # Return
/// TCL friendly version of `s`
fn tclize(s: &str) -> String {
    if s.chars().any(|c| c.is_whitespace()) {
        String::new() + "\"" + &s.replace('\"', "\\\"") + "\""
    } else {
        s.to_string()
    }
}

/// Print one line of output
///
/// # Argument
/// * `s` String representing one output line
/// # Return
/// Error if argument *s* represents more than one line
fn print_str(s: &str) -> Result<(), String> {
    if s.lines().count() > 1 {
        return Err(format!("cannot handle multiline output:\n{}", s.yellow()));
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
fn print_var(op: &str, var: &str) -> Result<(), String> {
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
fn print_var_val(op: &str, var: &str, val: &str, drop_empty: bool) -> Result<(), String> {
    if !val.is_empty() {
        print_str(&format!("{} {} {}", op, var, &tclize(val)))?
    } else if !drop_empty {
        print_str(&format!("{} {} \"\"", op, var))?
    } else {
        eprintln!("# DROPPED: {} {}", op, var.cyan());
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
fn print_header(vars: &BTreeMap<String, String>) -> Result<(), String> {
    use std::env::args;

    let comment = args().fold(String::from("# produced by:"), |s, arg| s + " " + &arg);
    print_str(&comment)?;
    for (var, val) in vars.iter() {
        print_var_val("set", var, val, true)?
    }
    Ok(())
}

/// Handle path environment variable
///
/// # Argument
/// * `var` Variable name
/// * `sep` Path separator
/// * `before` Environment before change
/// * `after` Environment after change
/// * `transform` Transform arguments
fn handle_path_var(
    var: &str,
    sep: &str,
    before: &BTreeMap<String, String>,
    after: &BTreeMap<String, String>,
    transform: &Transform,
) -> Result<(), String> {
    if before.contains_key(var) {
        if after.contains_key(var) {
            let val_before = before.get(var).unwrap().trim();
            let val_after = after.get(var).unwrap().trim();
            if val_before != val_after {
                if val_after.starts_with(val_before) {
                    let delta = val_after.get(val_before.len()..val_after.len()).unwrap();
                    if delta.starts_with(sep) ^ val_before.ends_with(sep) {
                        print_var_val("append-path", var, &map_path(delta, sep, transform)?, true)?
                    } else {
                        eprintln!(
                            "# WARNING: unsupported path suffix in variable: {}",
                            var.cyan()
                        )
                    }
                } else if val_after.ends_with(val_before) {
                    let delta = val_after
                        .get(0..val_after.len() - val_before.len())
                        .unwrap();
                    if delta.ends_with(sep) ^ val_before.starts_with(sep) {
                        print_var_val("prepend-path", var, &map_path(delta, sep, transform)?, true)?
                    } else {
                        eprintln!(
                            "# WARNING: unsupported path prefix in variable: {}",
                            var.cyan()
                        );
                        eprintln!("#          before: {}", val_before);
                        eprintln!("#          after: {}", val_after)
                    }
                } else if val_after.contains(val_before) {
                    let start_end = val_after.split(val_before).collect::<Vec<&str>>();
                    if start_end.len() != 2 {
                        eprintln!(
                            "# WARNING: ignoring unexpected change in variable: {}",
                            var.cyan()
                        );
                    }
                    print_var_val(
                        "prepend-path",
                        var,
                        &map_path(start_end[0], sep, transform)?,
                        true,
                    )?;
                    print_var_val(
                        "append-path",
                        var,
                        &map_path(start_end[1], sep, transform)?,
                        true,
                    )?
                } else {
                    eprintln!(
                        "# WARNING: ignoring unsupported change in path ariable: {}",
                        var.cyan()
                    );
                    eprintln!("#          before: {}", val_before);
                    eprintln!("#          after: {}", val_after)
                }
            }
        } else {
            print_var("unsetenv", var)?
        }
    } else {
        let path = map_path(after.get(var).unwrap(), sep, transform)?;
        print_var_val("prepend-path", var, &path, true)?
    }
    Ok(())
}

/// Handle normal environment variable
///
/// # Argument
/// * `var` Variable name
/// * `before` Environment before change
/// * `after` Environment after change
fn handle_normal_var(
    var: &str,
    before: &BTreeMap<String, String>,
    after: &BTreeMap<String, String>,
) -> Result<(), String> {
    if before.contains_key(var) {
        if after.contains_key(var) {
            let val_before = before.get(var).unwrap();
            let val_after = after.get(var).unwrap();
            if val_before != val_after {
                eprintln!(
                    "# WARNING: ignoring unsupported change in normal variable: {}",
                    var.cyan()
                );
                eprintln!("#          before: {}", val_before);
                eprintln!("#          after: {}", val_after)
            }
        } else {
            print_var("unsetenv", var)?
        }
    } else {
        print_var_val("setenv", var, after.get(var).unwrap(), false)?
    }
    Ok(())
}

/// Run the program
fn run() -> Result<(), String> {
    let cli_args = Args::parse();

    let exceptions = cli_args.except;
    let replacements = parse_replacements(cli_args.replace)?;
    let variables = parse_variables(cli_args.prefix, cli_args.var)?;
    let mut vartypes = parse_vartypes(cli_args.typ)?;
    let drops = cli_args.drop;
    let check_path = cli_args.check_path;
    print_header(&variables)?;

    let mut before = BTreeMap::new();
    parse_env(&mut before);
    let mut after = BTreeMap::new();
    for (var, val) in std::env::vars() {
        after.insert(var, val);
    }
    let vars_before: BTreeSet<&str> = before
        .keys()
        .map(|s| s.as_str())
        .collect::<BTreeSet<&str>>();
    let vars_after: BTreeSet<&str> = after.keys().map(|s| s.as_str()).collect::<BTreeSet<&str>>();
    let mut vars: BTreeSet<&str> = vars_before
        .union(&vars_after)
        .cloned()
        .collect::<BTreeSet<&str>>();
    exceptions.into_iter().for_each(|var| {
        vars.remove(var.as_str());
    });
    update_vartypes(&mut vartypes, &vars);
    let transform = Transform {
        drops: drops.as_slice(),
        replacements: &replacements,
        vars: &variables,
        check_path,
    };

    for var in vars {
        if is_path(var, &vartypes) {
            let sep = vartypes.get(var).unwrap().as_ref().unwrap_or_else(|| {
                panic!(
                    "internal error: expected nonempty separator for variable {}",
                    var
                )
            });
            handle_path_var(var, sep, &before, &after, &transform)?;
        } else {
            handle_normal_var(var, &before, &after)?;
        }
    }
    Ok(())
}

/// Run program and handle errors
fn main() {
    if let Err(msg) = run() {
        eprintln!("{} {}", "error:".red(), msg)
    }
}
