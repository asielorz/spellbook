#![windows_subsystem = "windows"]

use clap::{Parser, Subcommand};
use http_body_util::Full;
use hyper::body::{Bytes, Incoming};
use hyper::server::conn::http1;
use hyper::service::service_fn;
use hyper::{Method, Request, Response};
use hyper_util::rt::TokioIo;
use std::net::SocketAddr;
use tokio::net::TcpListener;

mod auth;
mod config;
mod requests;
mod run;

async fn process_request(req: Request<Incoming>) -> Result<Response<Full<Bytes>>, hyper::Error> {
    if let Some(query) = req.uri().query() {
        println!("Request {} at path {}?{}", req.method(), req.uri().path(), query);
    } else {
        println!("Request {} at path {}", req.method(), req.uri().path());
    }

    match (req.method(), req.uri().path()) {
        (&Method::GET, "/favicon.ico") => requests::serve_file("pages/favicon.ico", "image/vnd.microsoft.icon", true),
        (&Method::GET, "/page.js") => requests::serve_file("pages/page.js", "text/javascript", true),
        (&Method::GET, path) if path.starts_with("/fontawesome/") => requests::serve_file(
            &format!("pages/{}", path),
            if path.ends_with(".css") { "text/css" } else { "font/ttf" },
            true,
        ),
        (&Method::GET, path) if path.starts_with("/file/") => {
            let encoded_file_path = &path["/file/".len()..];
            let file_path = percent_encoding::percent_decode_str(encoded_file_path).decode_utf8_lossy();
            requests::serve_index_html_page(&file_path)
        }
        (&Method::GET, path) if path.starts_with("/api/file/") => {
            let encoded_file_path = &path["/api/file/".len()..];
            let file_path = percent_encoding::percent_decode_str(encoded_file_path).decode_utf8_lossy();
            let is_file = std::fs::metadata(file_path.as_ref())
                .map(|m| m.is_file())
                .unwrap_or(false);
            let is_markdown = file_path.ends_with(".md");
            if is_file && is_markdown {
                requests::serve_file(&file_path, "text/plain; charset=utf-8", false)
            } else {
                requests::not_found_404_response()
            }
        }
        (&Method::POST, "/api/run") => requests::run_code(req).await,

        // Return the 404 Not Found for other routes.
        _ => requests::not_found_404_response(),
    }
}

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Args {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    Server {
        /// Port to listen on
        #[arg(short, long, value_name = "u16", default_value_t = 8081)]
        port: u16,
    },
    Open {
        /// File to open
        path: String,
    },
}

#[tokio::main]
pub async fn main() -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let args = Args::parse();

    match args.command {
        Command::Open { path } => open(&path),
        Command::Server { port } => server(port).await,
    }
}

fn open(path: &str) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let encoded_path = percent_encoding::utf8_percent_encode(path, percent_encoding::NON_ALPHANUMERIC);
    Ok(open::that(format!("http://127.0.0.1:8081/file/{}", encoded_path))?)
}

async fn server(port: u16) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let config_path = match dirs::home_dir() {
        Some(dir) => {
            let mut osstr = dir.into_os_string();
            osstr.push("/.spellbook.json");
            osstr
        }
        None => {
            println!("Could not find user directory in the system. The program will now close.");
            return Ok(());
        }
    };
    println!("Config path: {}", config_path.to_str().unwrap());
    let config = config::load_config_file(config_path)?;
    run::install_runners(config.runners);

    let addr = SocketAddr::from(([127, 0, 0, 1], port));
    let listener = TcpListener::bind(addr).await?;

    loop {
        let (stream, addr) = listener.accept().await?;

        // For security, ignore every request that doesn't come from localhost.
        if addr.ip().is_loopback() {
            let io = TokioIo::new(stream);

            tokio::task::spawn(async move {
                // Finally, we bind the incoming connection to our `hello` service
                if let Err(err) = http1::Builder::new()
                    // `service_fn` converts our function in a `Service`
                    .serve_connection(io, service_fn(process_request))
                    .await
                {
                    eprintln!("Error serving connection: {:?}", err);
                }
            });
        }
    }
}
