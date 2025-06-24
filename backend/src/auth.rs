use lazy_static::lazy_static;
use std::{
    collections::{HashMap, hash_map::Entry},
    sync::Mutex,
    time::{Duration, Instant},
};

pub fn authorize(path: &str) -> u64 {
    let timestamp = std::time::Instant::now();
    let mut tokens = TOKENS.lock().unwrap();
    prune(&mut tokens, timestamp, TOKEN_DURATION);

    loop {
        let token: u64 = rand::random();
        if token != 0 {
            if let Entry::Vacant(e) = tokens.entry(token) {
                e.insert(Token {
                    path: path.into(),
                    timestamp,
                });
                return token;
            }
        }
    }
}

pub fn is_authorized(path: &str, token: u64) -> bool {
    if token == 0 {
        return false;
    }

    let now = std::time::Instant::now();
    let mut tokens = TOKENS.lock().unwrap();
    prune(&mut tokens, now, TOKEN_DURATION);

    let Some(entry) = tokens.get(&token) else { return false };
    is_up_to_date(entry, now, TOKEN_DURATION) && entry.path == path
}

const TOKEN_DURATION: Duration = Duration::from_secs(60 * 60);

struct Token {
    path: String,
    timestamp: Instant,
}

lazy_static! {
    static ref TOKENS: Mutex<HashMap<u64, Token>> = Mutex::default();
}

fn is_up_to_date(token: &Token, now: Instant, older_than: Duration) -> bool {
    token.timestamp - now < older_than
}

fn prune(tokens: &mut HashMap<u64, Token>, now: Instant, older_than: Duration) {
    tokens.retain(|_, token| is_up_to_date(token, now, older_than));
}
