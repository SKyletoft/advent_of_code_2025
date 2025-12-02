use std::io::Read;

fn parse(s: &str) -> i32 {
	let rest = &s[1..];
	match &s[0..1] {
		"R" => rest.parse::<i32>().unwrap(),
		"L" => -rest.parse::<i32>().unwrap(),
		_ => panic!("Invalid"),
	}
}

const fn wrap_add(x: i32, y: i32) -> i32 {
	(x + y).rem_euclid(100)
}

fn flatten(x: i32) -> impl Iterator<Item = i32> {
	std::iter::repeat_n(x.signum(), x.unsigned_abs() as usize)
}

fn part1(input: &str) -> usize {
	input
		.lines()
		.map(parse)
		.scan(50, |state, x| {
			*state = wrap_add(*state, x);
			Some(*state)
		})
		.filter(|&x| x == 0)
		.count()
}

fn part2(input: &str) -> usize {
	input
		.lines()
		.map(parse)
		.flat_map(flatten)
		.scan(50, |state, x| {
			*state = wrap_add(*state, x);
			Some(*state)
		})
		.filter(|&x| x == 0)
		.count()
}

fn main() {
	let input = {
		let mut buf = String::new();
		std::io::stdin().read_to_string(&mut buf).unwrap();
		buf
	};
	println!("Part 1: {}\nPart 2: {}", part1(&input), part2(&input));
}
