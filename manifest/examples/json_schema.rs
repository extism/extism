use extism_manifest::Manifest;
use schemars::schema_for;

fn main() {
    let schema = schema_for!(Manifest);
    println!("{}", serde_json::to_string_pretty(&schema).unwrap());
}
