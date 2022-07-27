use std::fs;
use std::path::PathBuf;

pub fn file_or_files_in_dir(path: &String, extension: &str) -> Result<Vec<PathBuf>, String> {
    let mut input_files = vec![];

    if fs::metadata(&path)
        .map_err(|e| e.to_string() + ": " + &path)?
        .is_file()
    {
        input_files.push(PathBuf::from(path));
    } else {
        for path in fs::read_dir(path)
            .unwrap()
            .map(|p| p.unwrap().path())
            .filter(|p| {
                p.extension()
                    .and_then(|ext| ext.to_str())
                    .map(|ext| ext.to_lowercase())
                    .map(|ext| ext == extension)
                    .unwrap_or(false)
            })
        {
            input_files.push(path);
        }
    }

    Ok(input_files)
}
