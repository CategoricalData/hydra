// hydra.lib.literals primitives

pub fn show_int32(n: i32) -> String { n.to_string() }
pub fn show_string(s: String) -> String { format!("{:?}", s) }
pub fn show_float64(f: f64) -> String { f.to_string() }
pub fn show_bigint(n: String) -> String { n }
pub fn bigfloat_to_float64(f: f64) -> f64 { f }
pub fn float32_to_bigfloat(f: f32) -> f64 { f as f64 }
pub fn int8_to_bigint(n: i8) -> String { n.to_string() }
pub fn int16_to_bigint(n: i16) -> String { n.to_string() }
pub fn int32_to_bigint(n: i32) -> String { n.to_string() }
pub fn int64_to_bigint(n: i64) -> String { n.to_string() }
pub fn uint8_to_bigint(n: u8) -> String { n.to_string() }
pub fn uint16_to_bigint(n: u16) -> String { n.to_string() }
pub fn uint32_to_bigint(n: u32) -> String { n.to_string() }
pub fn uint64_to_bigint(n: u64) -> String { n.to_string() }
