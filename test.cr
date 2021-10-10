mod math {
	fn mul(a: i32, b: i32) -> i32 {
		let out = 0;

		while b != 0 {
			out += a;
			b -= 1;
		}

		return out;
	}

	fn factorial(x: i32) -> i32 {
		let out = 0;

		if x == 1 {
			fn ONE() -> i32 {
				return 1;
			}

			out = ONE();
		} else {
			out = mul(x, factorial(x - 1));
		}

		return out;
	}
}

use math::factorial;

let x = factorial(4);
