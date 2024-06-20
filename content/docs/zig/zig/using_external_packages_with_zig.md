---
title: "Third Party Dependencies in Zig"
description: "Zig is a general-purpose programming language and toolchain for maintaining robust, optimal, and reusable software."
icon: "code"
draft: false
---

Zig's built-in package manager is relatively new and, as a consequence, has a number of rough edges. While there is room for improvements, it's usable as is. There are two parts we need to look at: creating a package and using packages. We'll go through this in full.

First, create a new folder named calc and create three files. The first is add.zig, with the following content:

```zig
// Oh, a hidden lesson, look at the type of b
// and the return type!!

pub fn add(a: anytype, b: @TypeOf(a)) @TypeOf(a) {
	return a + b;
}

const testing = @import("std").testing;
test "add" {
	try testing.expectEqual(@as(i32, 32), add(30, 2));
}
```

It's a bit silly, a whole package just to add two values, but it will let us focus on the packaging aspect. Next we'll add an equally silly: calc.zig:

```zig
pub const add = @import("add.zig").add;

test {
	// By default, only tests in the specified file
	// are included. This magic line of code will
	// cause a reference to all nested containers
	// to be tested.
	@import("std").testing.refAllDecls(@This());
}
```

We're splitting this up between calc.zig and add.zig to prove that zig build will automatically build and package all of our project files. Finally, we can add a build.zig:

```zig
const std = @import("std");

pub fn build(b: *std.Build) !void {
	const target = b.standardTargetOptions(.{});
	const optimize = b.standardOptimizeOption(.{});

	const tests = b.addTest(.{
		.target = target,
		.optimize = optimize,
		.root_source_file = b.path("calc.zig"),
	});

	const test_cmd = b.addRunArtifact(tests);
	test_cmd.step.dependOn(b.getInstallStep());
	const test_step = b.step("test", "Run the tests");
	test_step.dependOn(&test_cmd.step);
}
```

This is all a repetition of what we saw in the previous section. With this, you can run zig build test --summary all.

Back to our learning project and our previously created build.zig. We'll begin by adding our local calc as a dependency. We need to make three additions. First, we'll create a module pointing to our calc.zig:

```zig
// You can put this near the top of the build
// function, before the call to addExecutable.

const calc_module = b.addModule("calc", .{
	.root_source_file = b.path("PATH_TO_CALC_PROJECT/calc.zig"),
});
```

You'll need to adjust the path to calc.zig. We now need to add this module to both our existing exe and tests variables. Since our build.zig is getting busier, we'll try to organize things a little:

```zig
const std = @import("std");

pub fn build(b: *std.Build) !void {
	const target = b.standardTargetOptions(.{});
	const optimize = b.standardOptimizeOption(.{});

	const calc_module = b.addModule("calc", .{
		.root_source_file = b.path("PATH_TO_CALC_PROJECT/calc.zig"),
	});

	{
		// setup our "run" cmmand

		const exe = b.addExecutable(.{
			.name = "learning",
			.target = target,
			.optimize = optimize,
			.root_source_file = b.path("learning.zig"),
		});
		// add this
		exe.root_module.addImport("calc", calc_module);
		b.installArtifact(exe);

		const run_cmd = b.addRunArtifact(exe);
		run_cmd.step.dependOn(b.getInstallStep());

		const run_step = b.step("run", "Start learning!");
		run_step.dependOn(&run_cmd.step);
	}

	{
		// setup our "test" command
		const tests = b.addTest(.{
			.target = target,
			.optimize = optimize,
			.root_source_file = b.path("learning.zig"),
		});
		// add this
		tests.root_module.addImport("calc", calc_module);

		const test_cmd = b.addRunArtifact(tests);
		test_cmd.step.dependOn(b.getInstallStep());
		const test_step = b.step("test", "Run the tests");
		test_step.dependOn(&test_cmd.step);
	}
}
```

From within your project, you're now able to @import("calc"):

```zig
const calc = @import("calc");
...
calc.add(1, 2);
```

Adding a remote dependency takes a bit more effort. First, we need to go back to the calc project and define a module. You might think that the project itself is a module, but a project can expose multiple modules, so we need to explicitly create it. We use the same addModule, but discard the return value. Simply calling addModule is enough to define the module which other projects will then be able to import.

```zig
_ = b.addModule("calc", .{
	.root_source_file =  b.path("calc.zig"),
});
```

This is the only change we need to make to our library. Because this is an exercise in having a remote dependency, I've pushed this calc project to Github so that we can import it into our learning project. It's available at https://github.com/karlseguin/calc.zig.

Back in our learning project, we need a new file, build.zig.zon. "ZON" stands for Zig Object Notation and it allows Zig data to be expressed in a human readable format, and for that human readable format to be turned into Zig code. The contents of the build.zig.zon will be:

```zig
.{
  .name = "learning",
  .paths = .{""},
  .version = "0.0.0",
  .dependencies = .{
    .calc = .{
      .url = "https://github.com/karlseguin/calc.zig/archive/d1881b689817264a5644b4d6928c73df8cf2b193.tar.gz",
      .hash = "12ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
    },
  },
}
```

There are two questionable values in this file, the first is d1881b689817264a5644b4d6928c73df8cf2b193 within the url. This is simply the git commit hash. The second is the value of hash. As far as I know, there's currently no great way to tell what this value should be, so we use a dummy value for the time being.

To use this dependency, we need to make one change to our build.zig:

```zig
// replace this:
const calc_module = b.addModule("calc", .{
	.root_source_file = b.path("calc/calc.zig"),
});

// with this:
const calc_dep = b.dependency("calc", .{.target = target,.optimize = optimize});
const calc_module = calc_dep.module("calc");
```

In build.zig.zon we named the dependency calc, and that's the dependency that we're loading here. From within this dependency, we're grabbing the calc module, which is what we named the module in calc's build.zig.

If you try to run zig build test, you should see an error:

```bash
hash mismatch: manifest declares
122053da05e0c9348d91218ef015c8307749ef39f8e90c208a186e5f444e818672da
```

but the fetched package has
`122036b1948caa15c2c9054286b3057877f7b152a5102c9262511bf89554dc836ee5`
Copy and paste the correct hash back into the build.zig.zon and try running zig build test again. Everything should now be working.

It sounds like a lot, and I hope things get streamlined. But it's mostly something you can copy and paste from other projects and, once setup, you can move on.

A word of warning, I've found Zig's caching of dependencies to be on the aggressive side. If you try to update a dependency but Zig doesn't seem to detect the change...well, I nuke the project's zig-cache folder as well as ~/.cache/zig.

### Learn How To Build AI Projects

Now, if you are interested in upskilling in 2024 with AI development, check out this 6 AI advanced projects with Go where you learng about building with AI and getting the best knowledge there is currently. Here's the [link](https://akhilsharmatech.gumroad.com/l/zgxqq).