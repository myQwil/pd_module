const std = @import("std");

const Options = struct {
	float_size: u8 = 32,
};

pub fn build(b: *std.Build) !void {
	const target = b.standardTargetOptions(.{});
	const optimize = b.standardOptimizeOption(.{});

	const default: Options = .{};
	const opt: Options = .{
		.float_size = b.option(u8, "float_size",
			"Size of a floating-point number"
		) orelse default.float_size,
	};

	const opts = b.addOptions();
	opts.addOption(u8, "float_size", opt.float_size);

	_ = b.addModule("pd", .{
		.root_source_file = b.path("src/pd.zig"),
		.imports = &.{.{ .name = "options", .module = opts.createModule() }},
		.target = target,
		.optimize = optimize,
	});
}
