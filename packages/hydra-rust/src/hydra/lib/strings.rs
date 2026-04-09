// hydra.lib.strings primitives

pub fn cat(ss: Vec<String>) -> String {
    ss.concat()
}

pub fn cat2(s1: String, s2: String) -> String {
    format!("{}{}", s1, s2)
}

pub fn from_list(cs: Vec<i32>) -> String {
    cs.into_iter().filter_map(|c| char::from_u32(c as u32)).collect()
}

pub fn intercalate(sep: String, ss: Vec<String>) -> String {
    ss.join(&sep)
}

pub fn is_empty(s: String) -> bool {
    s.is_empty()
}

pub fn length(s: String) -> i32 {
    s.len() as i32
}

pub fn lines(s: String) -> Vec<String> {
    s.lines().map(String::from).collect()
}

pub fn split_on(sep: String, s: String) -> Vec<String> {
    s.split(&sep).map(String::from).collect()
}

pub fn to_list(s: String) -> Vec<i32> {
    s.chars().map(|c| c as i32).collect()
}

pub fn to_lower(s: String) -> String {
    s.to_lowercase()
}

pub fn to_upper(s: String) -> String {
    s.to_uppercase()
}

pub fn unlines(ss: Vec<String>) -> String {
    ss.join("\n")
}
