pub(crate) fn read_mur_file(path: impl AsRef<std::path::Path>) -> Result<String, String> {
    let path = path.as_ref();
    let src = std::fs::read_to_string(path)
        .map_err(|read_err| format!("'{p}' {read_err}", p = path.display()))?;

    Ok(src)
}

pub(crate) fn trim_end(s: &mut String) {
    if let Some(end_idx) = s.rfind(|c: char| !c.is_whitespace()) {
        s.truncate(end_idx + 1);
    } else {
        s.clear();
    }
}
pub(crate) fn trim_start(s: &mut String) {
    if let Some(start_idx) = s.find(|c: char| !c.is_whitespace()) {
        let _ = s.drain(..start_idx);
    }
}

pub(crate) fn trim(s: &mut String) {
    trim_start(s);
    trim_end(s);
}
