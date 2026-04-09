// hydra.lib.chars primitives

pub fn is_alpha_num(c: i32) -> bool {
    char::from_u32(c as u32).map_or(false, |ch| ch.is_alphanumeric())
}

pub fn is_lower(c: i32) -> bool {
    char::from_u32(c as u32).map_or(false, |ch| ch.is_lowercase())
}

pub fn is_space(c: i32) -> bool {
    char::from_u32(c as u32).map_or(false, |ch| ch.is_whitespace())
}

pub fn is_upper(c: i32) -> bool {
    char::from_u32(c as u32).map_or(false, |ch| ch.is_uppercase())
}

pub fn to_lower(c: i32) -> i32 {
    char::from_u32(c as u32)
        .map(|ch| ch.to_lowercase().next().unwrap_or(ch) as i32)
        .unwrap_or(c)
}

pub fn to_upper(c: i32) -> i32 {
    char::from_u32(c as u32)
        .map(|ch| ch.to_uppercase().next().unwrap_or(ch) as i32)
        .unwrap_or(c)
}
