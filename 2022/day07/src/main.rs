use std::collections::HashMap;
use std::fmt;

enum File {
    File(usize),
    Dir(HashMap<String, File>),
}

impl File {
    fn get_size(&self) -> usize {
        match self {
            File::File(size) => *size,
            File::Dir(children) => children
                .iter()
                .map(|(_, val)| val)
                .map(File::get_size)
                .sum(),
        }
    }

    fn insert_file(&mut self, path: &[&str], name: String, file: File) {
        if let File::Dir(map) = self {
            if path.is_empty() {
                map.entry(name).or_insert(file);
            } else {
                let remaining_path = &path[1..];
                map.get_mut(path[0])
                    .unwrap()
                    .insert_file(remaining_path, name, file);
            }
        } else {
            panic!("Tried to insert a file into a file which was not a dir")
        }
    }

    fn count_dir_size_under_limit(&self, limit: usize) -> usize {
        match self {
            File::File(_) => 0,
            File::Dir(children) => {
                let size = self.get_size();

                let children_result = children
                    .values()
                    .map(|f| f.count_dir_size_under_limit(limit))
                    .sum::<usize>();

                children_result + if size < limit { size } else { 0 }
            }
        }
    }

    fn get_smallest_dir_with_size(&self, limit: usize) -> Option<(&File, usize)> {
        if let File::Dir(children) = self {
            let size = self.get_size();
            if size < limit {
                return None;
            }

            let result = children
                .values()
                .flat_map(|f| f.get_smallest_dir_with_size(limit))
                .min_by_key(|(_, size)| *size);

            result.or(Some((self, size)))
        } else {
            None
        }
    }

    fn print_rec(
        &self,
        f: &mut fmt::Formatter<'_>,
        indent: usize,
        current_name: &str,
    ) -> fmt::Result {
        write!(f, "{} - ", " ".repeat(indent))?;

        match self {
            File::File(size) => writeln!(f, "{current_name} (file, size={size})")?,
            File::Dir(children) => {
                writeln!(f, "{current_name} (dir)")?;
                for (name, child) in children {
                    child.print_rec(f, indent + 1, name)?;
                }
            }
        }

        Ok(())
    }
}

impl fmt::Display for File {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.print_rec(f, 0, "/")
    }
}

fn main() {
    let input = common::read_lines("data/input.txt").expect("File could not be read");

    let mut iter = input.iter().peekable();
    let mut current_path = Vec::new();
    let mut fs = File::Dir(HashMap::new());

    while let Some(line) = iter.next() {
        if line.starts_with("$ cd") {
            // ignore root (/)
            if line.starts_with("$ cd /") {
                continue;
            }

            if line.starts_with("$ cd ..") {
                current_path.pop();
            } else {
                let name = line[5..].to_string();
                fs.insert_file(&current_path, name, File::Dir(HashMap::new()));
                current_path.push(&line[5..]);
            }
        } else if line.starts_with("$ ls") {
            while let Some(next) = iter.next_if(|next| !next.starts_with("$ ")) {
                if !next.starts_with("dir") {
                    let split = next.split(' ').collect::<Vec<_>>();
                    let size = split[0].parse::<usize>().expect("Could not parse size");
                    let name = split[1];

                    fs.insert_file(&current_path, name.into(), File::File(size));
                }
            }
        }
    }

    println!("{fs}");

    println!("{}", fs.count_dir_size_under_limit(100000));

    let fs_size = 70000000usize;
    let free_space = fs_size - fs.get_size();
    let missing_space = 30000000 - free_space;
    let (_, size) = fs.get_smallest_dir_with_size(missing_space).unwrap();

    println!("{size}");
}
