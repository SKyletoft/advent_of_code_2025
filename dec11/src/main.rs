use std::{collections::HashMap, io::Read};

type SmallVec<T> = smallvec::SmallVec<[T; 20]>;

fn main() {
	let input = {
		let mut s = String::new();
		std::io::stdin().read_to_string(&mut s).unwrap();
		s
	};
	let parsed = parse(&input);
	println!("part1: {}", part1(&parsed));
	println!("part2: {}", part2(&parsed));
}

const OUT: u16 = u16::MAX & 0b0011_1111_1111_1111;
const FFT: u16 = (u16::MAX - 1) & 0b0011_1111_1111_1111;
const DAC: u16 = (u16::MAX - 2) & 0b0011_1111_1111_1111;
const SVR: u16 = (u16::MAX - 3) & 0b0011_1111_1111_1111;
const YOU: u16 = (u16::MAX - 4) & 0b0011_1111_1111_1111;

fn parse(input: &str) -> HashMap<u16, SmallVec<u16>> {
	fn specials(val: &str) -> Option<u16> {
		match val {
			"out" => Some(OUT),
			"fft" => Some(FFT),
			"dac" => Some(DAC),
			"svr" => Some(SVR),
			"you" => Some(YOU),
			_ => None,
		}
	}

	let ids: HashMap<&str, u16> = input
		.lines()
		.map(|line| line.split([':', ' ']).find(|s| !s.is_empty()).unwrap())
		.enumerate()
		.map(|(x, y)| (y, x.try_into().unwrap()))
		.collect();
	input
		.lines()
		.map(|line| {
			let mut words = line.split([':', ' ']).filter(|s| !s.is_empty());
			let key = words.next().unwrap();
			let vals = words
				.map(|val| specials(val).unwrap_or_else(|| ids[val]))
				.collect::<SmallVec<u16>>();
			(specials(key).unwrap_or_else(|| ids[key]), vals)
		})
		.collect::<HashMap<u16, SmallVec<u16>>>()
}

fn part1(input: &HashMap<u16, SmallVec<u16>>) -> u64 {
	fn find_path(map: &HashMap<u16, SmallVec<u16>>, at: u16) -> u64 {
		if at == OUT {
			return 1;
		}
		let nexts = &map[&at];
		nexts.iter().map(|&next| find_path(map, next)).sum()
	}
	find_path(input, YOU)
}

fn part2(input: &HashMap<u16, SmallVec<u16>>) -> u64 {
	fn to_key(val: u16, a: bool, b: bool) -> u16 {
		debug_assert!(val & 0b1100_0000_0000_0000 == 0, "{val}");
		val | ((a as u16) << 15) | ((b as u16) << 14)
	}
	fn find_path_2(
		cache: &mut HashMap<u16, u64>,
		map: &HashMap<u16, SmallVec<u16>>,
		at: u16,
		mut seen_dac: bool,
		mut seen_fft: bool,
	) -> u64 {
		if let Some(ret) = cache.get(&to_key(at, seen_dac, seen_fft)) {
			return *ret;
		}
		seen_dac |= at == DAC;
		seen_fft |= at == FFT;
		if OUT == at {
			return (seen_dac && seen_fft) as _;
		}
		let nexts = &map[&at];
		let res = nexts
			.iter()
			.map(|&next| find_path_2(cache, map, next, seen_dac, seen_fft))
			.sum();
		cache.insert(to_key(at, seen_dac, seen_fft), res);
		res
	}
	find_path_2(&mut HashMap::new(), input, SVR, false, false)
}
