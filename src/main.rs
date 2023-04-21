#![warn(missing_docs)]

//! Program to produce modulefile lines by looking at Linux shell environment differences.

extern crate clap;
use clap::{crate_authors, crate_description, crate_name, crate_version, Parser};
use std::collections::HashMap;
use std::collections::HashSet;
use std::iter::FromIterator;
use thiserror::Error;

type GenericError = Box<dyn std::error::Error>;
type GenericResult<T> = Result<T, GenericError>;

extern crate text_colorizer;
use text_colorizer::Colorize;

/// Name of the prefix variable
///
/// The path represented by `PREFIX_VAR` can be set using the `--prefix=<path>` commandline option
const PREFIX_VAR: &str = "PREFIX";

/// Default path separator
///
/// The separator can be changes per path variable VAR using the `--typ=VAR:p:<separator>` commandline option
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
    #[arg(short = 'f', long, help = "Treat warnings as fatal")]
    fatal_warnings: bool,
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
struct Transform {
    drops: Vec<String>,
    replacements: Vec<Replacement>,
    vars: HashMap<String, String>,
    check_path: bool,
}

#[derive(Error, Debug)]
enum ProgramError {
    #[error("input not in /proc/self/environ format: {0}")]
    Input(String),
    #[error("unsupported non UTF-8 path: {0}")]
    NotUtf8(String),
    #[error("replacement: {0}")]
    Replacement(String),
    #[error("output: {0}")]
    Output(String),
    #[error("variable: {0}")]
    Variable(String),
    #[error("type: {0}")]
    Type(String),
    #[error("fatal: {0}")]
    Fatal(String),
}

/// Parse Linux shell environment from `io::stdin`
///
/// The input read from `io::stdin` is expected to be in the format of `/proc/self/environ`
///
/// # Return
/// Map var→val for all environment variables found
fn parse_env() -> GenericResult<HashMap<String, String>> {
    use std::io;
    use std::io::Read;

    let mut env_string = String::new();
    io::stdin().read_to_string(&mut env_string)?;
    env_string
        .trim_matches('\0')
        .split('\0')
        .map(|v| {
            v.split_once('=').map_or_else(
                || Err(Box::new(ProgramError::Input(v.to_string())) as GenericError),
                |p| Ok((p.0.to_string(), p.1.to_string())),
            )
        })
        .collect()
}

/// Check if variable is a path
///
/// # Argument
/// * `var` Variable name
/// * `typmap` Variable name to type map
/// # Return
/// true if the variable is in the typmap with a nonempty separator
fn is_path(var: &str, typmap: &HashMap<String, Option<String>>) -> bool {
    typmap.get(var).is_some()
}

/// Return canonic path
///
/// # Argument
/// * `path` String slice representing a file system path
/// # Returns
/// String containing the canonicalized path. If `path` cannot be canonicalized, it is returned as is.
fn to_canonic(path: &str) -> GenericResult<String> {
    use std::fs::canonicalize;

    canonicalize(path).map_or_else(
        |_| Ok(path.to_string()), // cannot canonicalize
        |p| {
            if let Some(s) = p.to_str() {
                Ok(s.to_string()) // canonicalized path
            } else {
                Err(Box::new(ProgramError::NotUtf8(path.cyan().to_string())) as GenericError)
                // path to utf8 conversion fails
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
/// * `transform` [Transform] arguments
/// # Returns
/// String containing the mapped path.
fn map_path(path: &str, sep: &str, transform: &Transform) -> GenericResult<String> {
    use std::path::Path;

    let mut path_list = Vec::new();
    for p in path.trim().split(sep).filter(|p| !p.is_empty()) {
        path_list.push(to_canonic(p)?)
    }
    if transform.check_path {
        path_list.retain(|p| Path::new(p).exists())
    }
    for d in &transform.drops {
        path_list.retain(|p| !p.contains(d))
    }
    for r in &transform.replacements {
        for p in &mut path_list {
            *p = p.replace(&r.0, &r.1)
        }
    }
    for (var, val) in &transform.vars {
        for p in &mut path_list {
            *p = p.replace(val, &format!("${{{}}}", var))
        }
    }
    Ok(path_list.join(sep))
}

/// Parse replacement arguments
///
/// This creates a vector of [Replacement] structures.
/// The replacement option string `old:new` will result in the vector element `Replacement(old, new)`.
///
/// # Argument
/// * `replacements` Replacement option strings
/// # Returns
/// Vector of [Replacement] structures
fn parse_replacements(replacements: Vec<String>) -> GenericResult<Vec<Replacement>> {
    replacements
        .iter()
        .map(|r| match r.split_once(':') {
            None | Some(("", _)) => Err(Box::new(ProgramError::Replacement(format!(
                "unsupported replacement {}, use OLD:NEW",
                r.yellow()
            ))) as GenericError),
            Some((old, new)) => Ok(Replacement(old.to_string(), new.to_string())),
        })
        .collect()
}

/// Parse variable substitutions
///
/// Create a variable name to variable value mapping.
/// The variable subtitution string `var=val` will result in the mapping
///
/// var→val
///
/// # Argument
/// * `vars` Variable substitution strings
/// # Return
/// Mapping from variable names to variable values, including [PREFIX_VAR] if defined
fn parse_variables(
    prefix: Option<String>,
    vars: Vec<String>,
) -> GenericResult<HashMap<String, String>> {
    let mut btree = HashMap::new();
    prefix.and_then(|value| btree.insert(PREFIX_VAR.to_string(), value));
    for v in vars {
        match v.split_once('=') {
            Some(("", "")) | None => Err(Box::new(ProgramError::Variable(format!(
                "definition {}, use VAR=VALUE",
                v.yellow()
            ))) as GenericError),
            Some(("", _)) => Err(Box::new(ProgramError::Variable(format!(
                "first part of {}, use VAR=VALUE",
                v.yellow()
            ))) as GenericError),
            Some((_, "")) => Err(Box::new(ProgramError::Variable(format!(
                "second part of {}, use VAR=VALUE",
                v.cyan()
            ))) as GenericError),
            Some((var, val)) => {
                if btree.insert(var.to_string(), val.to_string()).is_some() {
                    Err(Box::new(ProgramError::Variable(format!(
                        "duplicate definition of variable {}",
                        var.cyan()
                    ))) as GenericError)
                } else {
                    Ok(())
                }
            }
        }?
    }
    Ok(btree)
}

/// Parse variable types
///
/// Create a variable name to variable type mapping.
/// The variable type string `var:t[:s]` will result in
/// * var→None for t="n"
/// * var→Some(s) for t="p"
///
/// The default path separator is s=":"
/// # Argument
/// * `types` Variable type definitions
/// # Return
/// Mapping from variable names to variable types
fn parse_vartypes(types: Vec<String>) -> GenericResult<HashMap<String, Option<String>>> {
    let mut btree = HashMap::new();
    for v in types {
        let vardef = v.split_once(':');
        let typ = match vardef {
            Some(("", "")) | None => Err(Box::new(ProgramError::Type(format!(
                "definition {}, use VAR:TYPE[:SEP]",
                v.yellow()
            ))) as GenericError),
            Some(("", _)) => Err(Box::new(ProgramError::Type(format!(
                "first part of {}, use VAR:TYPE[:SEP]",
                v.cyan()
            ))) as GenericError),
            Some((_, "")) => Err(Box::new(ProgramError::Type(format!(
                "second part of {}, use VAR:TYPE[:SEP]",
                v.cyan()
            ))) as GenericError),
            Some((_var, "n")) => Ok(None),
            Some((_var, "p")) => Ok(Some(PATH_SEP)),
            Some((var, typdef)) => match typdef.split_once(':') {
                Some(("n", _)) => Err(Box::new(ProgramError::Type(format!(
                    "no separator allowed for {}:n",
                    var.cyan()
                ))) as GenericError),
                Some(("p", sep)) => Ok(Some(sep)),
                _ => Err(Box::new(ProgramError::Type(format!(
                    "TYPE {} for variable {} undefined, use one of 'n', 'p'",
                    typdef.yellow(),
                    var.cyan()
                ))) as GenericError),
            },
        }?;
        let var = vardef.unwrap().0;
        if btree
            .insert(var.to_string(), typ.map(|s| s.to_string()))
            .is_some()
        {
            return Err(Box::new(ProgramError::Type(format!(
                "duplicate definition of type for variable {}",
                var.cyan()
            ))));
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
fn update_vartypes(typmap: &mut HashMap<String, Option<String>>, vars: &HashSet<&str>) {
    for var in vars {
        if !typmap.contains_key(*var) {
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
///
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
fn print_str(s: &str) -> GenericResult<()> {
    if s.lines().count() > 1 {
        return Err(Box::new(ProgramError::Output(format!(
            "cannot handle multiline output:\n{}",
            s.yellow()
        ))));
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
fn print_var(op: &str, var: &str) -> GenericResult<()> {
    print_str(&format!("{} {}", op, var))
}

/// Binary operation on variable
///
/// # Argument
/// * `op` Binary operation name
/// * `var` Variable name
/// * `val` Operand
/// * `drop_empty` Drop operation if `val` is empty
/// # Examples
/// ```text
/// setenv HELLO 1
/// # DROPPED: append-path PATH
/// ```
fn print_var_val(op: &str, var: &str, val: &str, drop_empty: bool) -> GenericResult<()> {
    if !val.is_empty() {
        print_str(&format!("{} {} {}", op, var, &tclize(val)))
    } else if !drop_empty {
        print_str(&format!("{} {} \"\"", op, var))
    } else {
        print_str(&format!("# DROPPED: {} {}", op, var.cyan()))
    }
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
fn print_header(vars: &HashMap<String, String>) -> GenericResult<()> {
    use std::env::args;

    let comment = args().fold(String::from("# produced by:"), |s, arg| s + " " + &arg);
    print_str(&comment)?;
    for (var, val) in vars {
        print_var_val("set", var, val, true)?
    }
    Ok(())
}

/// Print warning for variable change
///
/// # Argument
/// * `val_before` Variable value before change
/// * `val_after` Variable value after change
/// * `message` Warning message
/// * `fatal` Raise error
/// # Example
/// ```text
/// # WARNING: ignoring unsupported change in normal variable: _
/// #          before: /usr/bin/cat
/// #          after: target/debug/pmodenv
/// ```
fn print_change_warning(
    val_before: &str,
    val_after: &str,
    message: &str,
    fatal: bool,
) -> GenericResult<()> {
    print_str(&format!("# WARNING: {}", message))?;
    print_str(&format!("#          before: {}", val_before))?;
    print_str(&format!("#          after: {}", val_after))?;
    if fatal {
        Err(Box::new(ProgramError::Fatal(message.to_string())))
    } else {
        Ok(())
    }
}

/// Handle path environment variable
///
/// # Argument
/// * `var` Name of a path (type 'p') variable
/// * `sep` Path separator
/// * `before` Environment before change
/// * `after` Environment after change
/// * `transform` [Transform] arguments
/// * `fatal_warnings` Treat warnings as fatal
fn handle_path_var(
    var: &str,
    sep: &str,
    before: &HashMap<String, String>,
    after: &HashMap<String, String>,
    transform: &Transform,
    fatal_warnings: bool,
) -> GenericResult<()> {
    match (before.get(var), after.get(var)) {
        (None, Some(val_after)) => print_var_val(
            "prepend-path",
            var,
            &map_path(val_after, sep, transform)?,
            true,
        )?,
        (Some(_), None) => print_var("unsetenv", var)?,
        (Some(val_before), Some(val_after)) => {
            if val_after == val_before {
                // no change
            } else if let Some(delta) = val_after.strip_prefix(val_before) {
                if delta.starts_with(sep) ^ val_before.ends_with(sep) {
                    print_var_val("append-path", var, &map_path(delta, sep, transform)?, true)?
                } else {
                    print_change_warning(
                        val_before,
                        val_after,
                        &format!("unsupported path suffix in variable: {}", var.cyan()),
                        fatal_warnings,
                    )?
                }
            } else if let Some(delta) = val_after.strip_suffix(val_before) {
                if delta.ends_with(sep) ^ val_before.starts_with(sep) {
                    print_var_val("prepend-path", var, &map_path(delta, sep, transform)?, true)?
                } else {
                    print_change_warning(
                        val_before,
                        val_after,
                        &format!("unsupported path prefix in variable: {}", var.cyan()),
                        fatal_warnings,
                    )?
                }
            } else if val_after.contains(val_before) {
                let start_end: Vec<&str> = val_after.split(val_before).collect();
                if start_end.len() != 2 {
                    print_change_warning(
                        val_before,
                        val_after,
                        &format!("ignoring unexpected change in variable: {}", var.cyan()),
                        fatal_warnings,
                    )?
                } else if start_end[0].ends_with(sep) && start_end[1].starts_with(sep) {
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
                    print_change_warning(
                        val_before,
                        val_after,
                        &format!("unsupported path prefix/suffix in variable: {}", var.cyan()),
                        fatal_warnings,
                    )?
                }
            } else {
                print_change_warning(
                    val_before,
                    val_after,
                    &format!(
                        "ignoring unsupported change in path ariable: {}",
                        var.cyan()
                    ),
                    fatal_warnings,
                )?
            }
        }
        _ => panic!("internal error: variable without value"),
    } // match
    Ok(())
}

/// Handle normal environment variable
///
/// # Argument
/// * `var` Name of a normal (non-path, type 'n') variable
/// * `before` Environment before change
/// * `after` Environment after change
/// * `fatal_warnings` Treat warnings as fatal
fn handle_normal_var(
    var: &str,
    before: &HashMap<String, String>,
    after: &HashMap<String, String>,
    fatal_warnings: bool,
) -> GenericResult<()> {
    match (before.get(var), after.get(var)) {
        (Some(val_before), Some(val_after)) => {
            if val_before != val_after {
                print_change_warning(
                    val_before,
                    val_after,
                    &format!(
                        "ignoring unsupported change in normal variable: {}",
                        var.cyan()
                    ),
                    fatal_warnings,
                )?
            }
        }
        (Some(_val_before), None) => print_var("unsetenv", var)?,
        (None, Some(val_after)) => print_var_val("setenv", var, val_after, false)?,
        _ => (),
    };
    Ok(())
}

/// Run the program
fn run() -> GenericResult<()> {
    let cli_args = Args::parse();

    let exceptions = cli_args.except;
    let replacements = parse_replacements(cli_args.replace)?;
    let variables = parse_variables(cli_args.prefix, cli_args.var)?;
    let mut vartypes = parse_vartypes(cli_args.typ)?;
    let drops = cli_args.drop;
    let check_path = cli_args.check_path;
    let fatal_warnings = cli_args.fatal_warnings;
    print_header(&variables)?;

    let before = parse_env()?;
    let after = HashMap::from_iter(std::env::vars());
    let vars_before: HashSet<&str> = HashSet::from_iter(before.keys().map(|s| s.as_str()));
    let vars_after: HashSet<&str> = HashSet::from_iter(after.keys().map(|s| s.as_str()));
    let mut vars = HashSet::from_iter(vars_before.union(&vars_after).copied());
    for var in exceptions {
        vars.remove(var.as_str());
    }
    update_vartypes(&mut vartypes, &vars);
    let transform = Transform {
        drops,
        replacements,
        vars: variables,
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
            handle_path_var(var, sep, &before, &after, &transform, fatal_warnings)?;
        } else {
            handle_normal_var(var, &before, &after, fatal_warnings)?;
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
