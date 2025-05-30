use std::{
    env, fs,
    path::{Path, PathBuf},
};

fn main() {
    //ship_dir("fonts");
}

fn ship_dir(path: &str) {
    let src_dir = Path::new(path);
    let out_dir = PathBuf::from(env::var("OUT_DIR").unwrap()).join(path);
    println!(
        "cargo:warning=copying fonts to {}",
        out_dir.to_string_lossy()
    );

    if out_dir.exists() {
        fs::remove_dir_all(&out_dir).unwrap();
    }

    copy_recursively(src_dir, &out_dir).unwrap();
    println!("cargo:rerun-if-changed={}", path);
}

fn copy_recursively(src: &Path, dst: &Path) -> std::io::Result<()> {
    for entry in fs::read_dir(src)? {
        let entry = entry?;
        let file_type = entry.file_type()?;
        let dst_path = dst.join(entry.file_name());

        if file_type.is_dir() {
            fs::create_dir_all(&dst_path)?;
            copy_recursively(&entry.path(), &dst_path)?;
        } else if file_type.is_file() {
            fs::create_dir_all(dst)?;
            fs::copy(entry.path(), dst_path)?;
        }
    }
    Ok(())
}
