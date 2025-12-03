use std::{fmt::Write, io::Read};

type SmallString = smallstr::SmallString<[u8; 20]>;

fn to_small_string(n: u64) -> SmallString {
	let mut s = SmallString::new();
	write!(&mut s, "{n}").unwrap();
	s
}

fn part1(input: &[(u64, SmallString)]) -> u64 {
	input
		.iter()
		.filter(|(_, n)| {
			let half_len = n.len() / 2;
			n.len() % 2 == 0 && n[..half_len] == n[half_len..]
		})
		.map(|(n, _)| n)
		.sum()
}

fn part2(input: &[(u64, SmallString)]) -> u64 {
	input
		.iter()
		.filter(|(_, n)| {
			(2..n.len()).any(|by| {
				let mut chunks = n.as_bytes().chunks(n.len() / by).peekable();
				let first = *chunks.peek().unwrap();
				n.len() % by == 0 && chunks.all(|c| c == first)
			})
		})
		.map(|(n, _)| n)
		.sum()
}

fn main() {
	let input = {
		let mut buf = String::new();
		std::io::stdin().read_to_string(&mut buf).unwrap();
		buf.trim_ascii()
			.split(',')
			.flat_map(|range| {
				let mut iter = range.split('-');
				let lo = iter.next().unwrap().parse::<u64>().unwrap();
				let hi = iter.next().unwrap().parse::<u64>().unwrap();
				lo..=hi
			})
			.map(|n| (n, to_small_string(n)))
			.collect::<Vec<_>>()
	};
	println!("Part 1: {}\nPart 2: {}", part1(&input), part2(&input));
}
