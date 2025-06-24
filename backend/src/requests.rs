use std::path::PathBuf;

use http_body_util::{BodyExt, Full};
use hyper::{
    Request, Response, StatusCode,
    body::{Bytes, Incoming},
};
use serde::Deserialize;

use crate::{auth, run};

pub fn serve_bytes(content: Vec<u8>, content_type: &str, cache: bool) -> Result<Response<Full<Bytes>>, hyper::Error> {
    Response::builder()
        .status(StatusCode::OK)
        .header("Access-Control-Allow-Origin", "*")
        .header("Access-Control-Allow-Headers", "*")
        .header(
            "Cache-Control",
            if cache {
                "public, max-age=31919000, immutable"
            } else {
                "max-age=0"
            },
        )
        .header("Content-Type", content_type)
        .body(Full::new(Bytes::from(content)))
        .or_else(|_| internal_server_error_response())
}

pub fn serve_file(path: &str, content_type: &str, cache: bool) -> Result<Response<Full<Bytes>>, hyper::Error> {
    match std::fs::read(path) {
        Ok(content) => serve_bytes(content, content_type, cache),
        Err(_) => internal_server_error_response(),
    }
}

pub fn serve_index_html_page(path: &str) -> Result<Response<Full<Bytes>>, hyper::Error> {
    let Ok(content) = std::fs::read_to_string("pages/index.html") else {
        return internal_server_error_response();
    };

    let token = auth::authorize(path);
    let patched_content = content.replace("__TOKEN__", &format!("\"{}\"", token));
    serve_bytes(patched_content.into_bytes(), "text/html; charset=utf-8", false)
}

#[derive(Deserialize)]
struct RunCodeRequest {
    path: String,
    token: String,
    language: String,
    code: String,
}
pub async fn run_code(req: Request<Incoming>) -> Result<Response<Full<Bytes>>, hyper::Error> {
    let Ok(whole_body) = req.into_body().collect().await else {
        return internal_server_error_response();
    };

    let request_object: RunCodeRequest = match serde_json::from_slice(&whole_body.to_bytes()) {
        Ok(x) => x,
        Err(err) => return bad_request_response(&format!("{}", err)),
    };

    let token: u64 = match request_object.token.parse() {
        Ok(x) => x,
        Err(err) => return bad_request_response(&format!("{}", err)),
    };

    if !auth::is_authorized(&request_object.path, token) {
        return unauthorized_response(
            "You are not authorized to run this code. Maybe your token has expired? Try reloading the page.",
        );
    }

    // As an extra verification step, we will make sure that the source code we got actually exists in the file.
    // The protection this provides is twofold. On one hand, we prevent arbitrary code execution over the network
    // by only executing code that actually exists in the file. On the other, we also avoid executing out of date
    // code that does not exist in the file anymore because it has changed.

    let file_content = match std::fs::read_to_string(&request_object.path) {
        Ok(content) => content,
        Err(_) => return bad_request_response(&format!("File {} could not be read", request_object.path)),
    };

    if !file_content.contains(&request_object.code) {
        return unauthorized_response(
            "Requested source code does not exist in the file anymore. Maybe the file has been modified? Try reloading the page.",
        );
    }

    println!("Run code ({}):", request_object.language);
    print!("{}", &request_object.code);
    if !request_object.code.ends_with("\n") {
        println!();
    }

    let cd = PathBuf::from(&request_object.path)
        .parent()
        .map(|p| p.to_owned())
        .unwrap_or_else(|| PathBuf::from(&request_object.path));

    if !run::run_code(&cd, &request_object.language, &request_object.code).await {
        return internal_server_error_response();
    }

    Ok(Response::builder()
        .status(StatusCode::OK)
        .body(Full::new(Bytes::new()))
        .unwrap())
}

fn internal_server_error_response() -> Result<Response<Full<Bytes>>, hyper::Error> {
    println!("500 Internal server error");
    Ok(Response::builder()
        .status(StatusCode::INTERNAL_SERVER_ERROR)
        .header("Access-Control-Allow-Origin", "*")
        .header("Access-Control-Allow-Headers", "*")
        .body(Full::new(Bytes::new()))
        .unwrap())
}

pub fn not_found_404_response() -> Result<Response<Full<Bytes>>, hyper::Error> {
    println!("404 Not found");
    Ok(Response::builder()
        .status(StatusCode::NOT_FOUND)
        .body(Full::new(Bytes::new()))
        .unwrap())
}

fn unauthorized_response(error_message: &str) -> Result<Response<Full<Bytes>>, hyper::Error> {
    println!("401 Unauthorized");
    Ok(Response::builder()
        .status(StatusCode::UNAUTHORIZED)
        .body(Full::from(format!(r#"{{"error_message":"{}""#, error_message)))
        .unwrap())
}

fn bad_request_response(error_message: &str) -> Result<Response<Full<Bytes>>, hyper::Error> {
    println!("Rejecting bad request: {}", error_message);

    Response::builder()
        .status(StatusCode::BAD_REQUEST)
        .header("Access-Control-Allow-Origin", "*")
        .header("Access-Control-Allow-Headers", "*")
        .header("Content-Type", "application/json")
        .body(Full::from(format!(r#"{{"error_message":"{}""#, error_message)))
        .or_else(|_| internal_server_error_response())
}
