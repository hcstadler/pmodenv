extern crate clap;
use std::env::args;
use clap::{Arg, App};
use std::collections::BTreeMap;
use std::fs::canonicalize;

const PREFIX_VAR: &str = "PREFIX";

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

fn to_canonic(path: &str) -> String
{
    if let Ok(p) = canonicalize(path) {
        p.to_str().unwrap().to_string()
    } else {
        path.to_string()
    }
}

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

fn main()
{
    use std::env;
    use std::collections::BTreeSet;

    print!("# produced by:");
    for a in args() {
        print!(" {}", a);
    }
    println!();
    let args = App::new("pmodenv")
                .version("1.0")
                .author("hans-christian.stadler@psi.ch")
                .about("Turns environment differences into a module file

  $ cat /proc/self/environ > /tmp/env.txt
  $ PATH=${HOME}/bin:${PATH} HELLO=1 ../target/debug/pmodenv -p ${HOME} -e _ < /tmp/env.txt
  # produced by: ../target/debug/pmodenv -p /home/stadler_h -e _
  set prefix /home/stadler_h
  setenv HELLO 1
  prepend-path PATH ${PREFIX}/bin")
                .arg(Arg::with_name("except")
                        .takes_value(true)
                        .multiple(true)
                        .short("e")
                        .long("except")
                        .value_name("VAR")
                        .help("ignores the environment variable"))
                .arg(Arg::with_name("prefix")
                        .takes_value(true)
                        .short("p")
                        .long("prefix")
                        .value_name("PREFIX")
                        .help("turns prefix into a variable"))
                .get_matches();
    let prefix = args.value_of("prefix");
    if let Some(path) = prefix {
        println!("set prefix {}", path);
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
    let exceptions = args.values_of("except");
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
