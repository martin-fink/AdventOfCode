fn main() {
    let input = common::read_file("data/input.txt").expect("File could not be read");
    
    let lines: Vec<&str> = input.split("\n").collect();

    let calories: Vec<u32> = lines.split(|s| s.is_empty())
    .map(|s| s.iter().map(|i| i.parse::<u32>().unwrap()).sum::<u32>())
    .collect();

    let mut max = [0u32; 3];

    for calorie in calories.iter() {
        if max[2] < *calorie {
            max[0] = max[1];
            max[1] = max[2];
            max[2] = *calorie;
        } else if max[1] < *calorie {
            max[0] = max[1];
            max[1] = *calorie;
        } else if max[0] < *calorie {
            max[0] = *calorie;
        }
    }

    println!("{}", max.iter().sum::<u32>());
}
