use serde::Deserialize;
use std::path::Path;

use crate::run::Runner;

pub fn load_config_file<P: AsRef<Path>>(path: P) -> Result<ConfigFile, Box<dyn std::error::Error + Send + Sync>> {
    let content = std::fs::read_to_string(path)?;
    let config: ConfigFile = serde_json::from_str(&content)?;
    Ok(config)
}

#[derive(Deserialize)]
pub struct ConfigFile {
    pub runners: Vec<Runner>,
}
