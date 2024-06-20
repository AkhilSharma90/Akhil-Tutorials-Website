---
title: "Generics in Zig"
description: "Zig is a general-purpose programming language and toolchain for maintaining robust, optimal, and reusable software."
icon: "code"
draft: false
---

Generics
Enter generics, the goal of which is to abstract algorithms and data structures from specific types.

Many languages implement generics with special syntax and generic-specific rules. With Zig, generics are less of a specific feature and more of an expression of what the language is capable of. Specifically, generics leverage Zig's powerful compile-time metaprogramming.

We'll begin by looking at a silly example, just to get our bearings:

```zig
const std = @import("std");

pub fn main() !void {
	var arr: IntArray(3) = undefined;
	arr[0] = 1;
	arr[1] = 10;
	arr[2] = 100;
	std.debug.print("{any}\n", .{arr});
}

fn IntArray(comptime length: usize) type {
	return [length]i64;
}
```

The above prints { 1, 10, 100 }. The interesting part is that we have a function that returns a type (hence the function is PascalCase). Not just any type either, but a type based on a function parameter. This code only worked because we declared length as comptime. That is, we require anyone who calls IntArray to pass a compile-time known length parameter. This is necessary because our function returns a type and types must always be compile-time known.

A function can return any type, not just primitives and arrays. For example, with a small change, we can make it return a structure:

```zig
const std = @import("std");

pub fn main() !void {
	var arr: IntArray(3) = undefined;
	arr.items[0] = 1;
	arr.items[1] = 10;
	arr.items[2] = 100;
	std.debug.print("{any}\n", .{arr.items});
}

fn IntArray(comptime length: usize) type {
	return struct {
		items: [length]i64,
	};
}
```

It might seem odd, but arr's type really is an IntArray(3). It's a type like any other type and arr is a value like any other value. If we called IntArray(7) that would be a different type. Maybe we can make things neater:

```zig
const std = @import("std");

pub fn main() !void {
	var arr = IntArray(3).init();
	arr.items[0] = 1;
	arr.items[1] = 10;
	arr.items[2] = 100;
	std.debug.print("{any}\n", .{arr.items});
}

fn IntArray(comptime length: usize) type {
	return struct {
		items: [length]i64,

		fn init() IntArray(length) {
			return .{
				.items = undefined,
			};
		}
	};
}
```

At first glance that might not look neater. But besides being nameless and nested in a function, our structure's looking like every other structure we've seen so far. It has fields, it has functions. You know what they say, if it looks like a duck.... Well, this looks, swims and quacks like a normal structure, because it is.

We've taken this route to get comfortable with a function that returns a type and the accompanying syntax. To get a more typical generic, we need to make one last change: our function has to take a type. In reality, this is a small change, but type can feel more abstract than usize, so we took it slowly. Let's make a leap and modify our previous IntList to work with any type. We'll start with a skeleton:

```zig
fn List(comptime T: type) type {
	return struct {
		pos: usize,
		items: []T,
		allocator: Allocator,

		fn init(allocator: Allocator) !List(T) {
			return .{
				.pos = 0,
				.allocator = allocator,
				.items = try allocator.alloc(T, 4),
			};
		}
	};
}
```

The above struct is almost identical to our IntList except i64 has been replaced with T. That T might seem special, but it's just a variable name. We could have called it item_type. However, following Zig's naming convention, variables of type type are PascalCase.

For good or bad, using a single letter to represent a type parameter is much older than Zig. T is a common default in most languages, but you will see context-specific variations, such as hash maps using K and V for their key and value parameter types.
If you aren't sure about our skeleton, consider the two places we use T: items: []T and allocator.alloc(T, 4). When we want to use this generic type, we'll create an instance using:

var list = try List(u32).init(allocator);
When the code gets compiled, the compiler creates a new type by finding every T and replacing it with u32. If we use List(u32) again, the compiler will re-use the type it previous created. If we specify a new value for T, say List(bool) or List(User), new types will be created.

To complete our generic List we can literally copy and paste the rest of the IntList code and replace i64 with T. Here's a full working example:

```zig
const std = @import("std");
const Allocator = std.mem.Allocator;

pub fn main() !void {
	var gpa = std.heap.GeneralPurposeAllocator(.{}){};
	const allocator = gpa.allocator();

	var list = try List(u32).init(allocator);
	defer list.deinit();

	for (0..10) |i| {
		try list.add(@intCast(i));
	}

	std.debug.print("{any}\n", .{list.items[0..list.pos]});
}

fn List(comptime T: type) type {
	return struct {
		pos: usize,
		items: []T,
		allocator: Allocator,

		fn init(allocator: Allocator) !List(T) {
			return .{
				.pos = 0,
				.allocator = allocator,
				.items = try allocator.alloc(T, 4),
			};
		}

		fn deinit(self: List(T)) void {
			self.allocator.free(self.items);
		}

		fn add(self: *List(T), value: T) !void {
			const pos = self.pos;
			const len = self.items.len;

			if (pos == len) {
				// we've run out of space
				// create a new slice that's twice as large
				var larger = try self.allocator.alloc(T, len * 2);

				// copy the items we previously added to our new space
				@memcpy(larger[0..len], self.items);

				self.allocator.free(self.items);

				self.items = larger;
			}

			self.items[pos] = value;
			self.pos = pos + 1;
		}
	};
}
```

Our init function returns a List(T), and our deinit and add functions take a List(T) and *List(T). In our simple class, that's fine, but for large data structures, writing the full generic name can become a little tedious, especially if we have multiple type parameters (e.g. a hash map that takes a separate type for its key and value). The @This() builtin function returns the innermost type from where it's called. Most likely, our List(T) would be written as:

```zig
fn List(comptime T: type) type {
	return struct {
		pos: usize,
		items: []T,
		allocator: Allocator,

		// Added
		const Self = @This();

		fn init(allocator: Allocator) !Self {
			// ... same code
		}

		fn deinit(self: Self) void {
			// .. same code
		}

		fn add(self: *Self, value: T) !void {
			// .. same code
		}
	};
}
```
Self isn't a special name, it's just a variable, and it's PascalCase because its value is a type. We can use Self where we had previously used List(T).

We could create more complex examples, with multiple type parameters and more advanced algorithms. But, in the end, the core generic code would be no different than the simple examples above. In the next part we'll touch on generics again when we look at the standard library's ArrayList(T) and StringHashMap(V).

### Learn How To Build AI Projects

Now, if you are interested in upskilling in 2024 with AI development, check out this 6 AI advanced projects with Go where you learng about building with AI and getting the best knowledge there is currently. Here's the [link](https://akhilsharmatech.gumroad.com/l/zgxqq).