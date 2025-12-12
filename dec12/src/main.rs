use std::{
	fmt::Display,
	io::Read,
	ops::{Index, IndexMut},
};

use smallvec::SmallVec;

fn main() {
	let input = {
		let mut s = String::new();
		std::io::stdin().read_to_string(&mut s).unwrap();
		s
	};

	println!("part1: {}", part1(&input));
}

#[derive(Default, Debug)]
struct Shape(u16);

impl Display for Shape {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let g = |x| if x { '#' } else { '.' };
		writeln!(
			f,
			"{}{}{}\n{}{}{}\n{}{}{}",
			g(self[(0, 0)]),
			g(self[(1, 0)]),
			g(self[(2, 0)]),
			g(self[(0, 1)]),
			g(self[(1, 1)]),
			g(self[(2, 1)]),
			g(self[(0, 2)]),
			g(self[(1, 2)]),
			g(self[(2, 2)]),
		)
	}
}

impl Shape {
	fn area(&self) -> u16 {
		self.0.count_ones() as _
	}
}

impl Index<(usize, usize)> for Shape {
	type Output = bool;

	fn index(&self, (x, y): (usize, usize)) -> &Self::Output {
		assert!(x < 3);
		assert!(y < 3);
		let idx = y * 3 + x;
		if self.0 & (1 << idx) != 0 {
			&true
		} else {
			&false
		}
	}
}

#[derive(Default, Debug)]
struct Canvas {
	width: usize,
	height: usize,
	canvas: Vec<bool>,
}

impl Display for Canvas {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		for line in self.canvas.chunks(self.width) {
			for col in line.iter().map(|&b| if b { '#' } else { '.' }) {
				write!(f, "{col}")?;
			}
			writeln!(f)?;
		}

		Ok(())
	}
}

impl Index<(usize, usize)> for Canvas {
	type Output = bool;

	fn index(&self, (x, y): (usize, usize)) -> &Self::Output {
		assert!(x < self.width);
		assert!(y < self.height);
		self.canvas.index(y * self.width + x)
	}
}

impl IndexMut<(usize, usize)> for Canvas {
	fn index_mut(&mut self, (x, y): (usize, usize)) -> &mut Self::Output {
		assert!(x < self.width);
		assert!(y < self.height);
		self.canvas.index_mut(y * self.width + x)
	}
}

fn part1(input: &str) -> u64 {
	let shapes: [Shape; 6] = {
		let mut s: [Shape; 6] = Default::default();
		input
			.split("\n\n")
			.take(6)
			.map(|block| {
				Shape(
					block
						.lines()
						.skip(1)
						.flat_map(|l| l.chars())
						.map(|c| (c == '#') as u16)
						.enumerate()
						.fold(0, |acc, (idx, curr)| acc | (curr << idx)),
				)
			})
			.zip(s.iter_mut())
			.for_each(|(s, ss)| {
				*ss = s;
			});
		s
	};

	let viable = input
		.lines()
		.skip(30)
		.filter(|line| {
			let parsed = line
				.split([' ', ':', 'x'])
				.filter(|s| !s.is_empty())
				.map(|x| x.parse::<u16>())
				.collect::<Result<SmallVec<[u16; 8]>, _>>()
				.unwrap();
			let [x, y, shape_counts @ ..] = parsed.as_slice() else {
				unreachable!()
			};
			let area = x * y;
			let required = shape_counts
				.iter()
				.zip(shapes.iter())
				.map(|(count, shape)| count * shape.area())
				.sum();
			area >= required
		})
		.count();

	viable as _
}
