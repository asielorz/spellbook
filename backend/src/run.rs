use lazy_static::lazy_static;
use serde::Deserialize;
use std::{
    collections::HashMap,
    hash::{Hash, Hasher},
    path::Path,
    sync::Mutex,
};

pub fn install_runners(runners: Vec<Runner>) {
    let mut global_runners = RUNNERS.lock().unwrap();
    for runner in runners {
        let key = runner.language.clone();
        global_runners.insert(key, runner);
    }
}

pub async fn run_code(cd: &Path, language: &str, code: &str) -> bool {
    let Some(runner) = RUNNERS.lock().unwrap().get(language).cloned() else {
        return false;
    };

    let hash = {
        let mut s = std::hash::DefaultHasher::new();
        language.hash(&mut s);
        code.hash(&mut s);
        s.finish()
    };

    let dir = format!("{}/spellbook", std::env::temp_dir().to_string_lossy());
    if std::fs::create_dir_all(&dir).is_err() {
        return false;
    }

    let path = format!("{}/{}.{}", dir, hash, runner.extension);

    if std::fs::write(&path, code).is_err() {
        return false;
    }

    let command_args: Vec<String> = runner.command.iter().map(|s| s.replace("__FILE__", &path)).collect();

    let mut command = tokio::process::Command::new(&command_args[0]);
    command.current_dir(cd);
    for arg in &command_args[1..] {
        command.arg(arg);
    }

    command.status().await.map(|o| o.success()).unwrap_or(false)
}

#[derive(Deserialize, Clone)]
pub struct Runner {
    language: String,
    command: Vec<String>, // Command should contain a __FILE__ constant, that will be replaced by the actual path to run.
    extension: String,
}

lazy_static! {
    static ref RUNNERS: Mutex<HashMap<String, Runner>> = Mutex::default();
}
