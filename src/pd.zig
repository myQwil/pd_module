const std = @import("std");
pub const imp = @import("imp.zig");
pub const cnv = @import("canvas.zig");
pub const iem = @import("all_guis.zig");
pub const stf = @import("stuff.zig");

const strlen = std.mem.len;

pub extern const pd_compatibilitylevel: c_int;

pub const Float = std.meta.Float(@import("options").float_size);
pub const Sample = Float;

pub const Method = ?*const fn () callconv(.c) void;
pub const NewMethod = ?*const fn () callconv(.c) ?*anyopaque;

pub const Class = imp.Class;
pub const GList = cnv.GList;
pub const Gui = iem.Gui;

pub const Word = extern union {
	// we're going to trust pd to give us valid pointers of the respective types
	float: Float,
	symbol: *Symbol,
	gpointer: *GPointer,
	array: *cnv.Array,
	binbuf: *BinBuf,
	index: c_int,
};


// ----------------------------------- Atom ------------------------------------
// -----------------------------------------------------------------------------
pub const Atom = extern struct {
	type: Type,
	w: Word,

	pub const Type = enum(c_uint) {
		none,
		float,
		symbol,
		pointer,
		semi,
		comma,
		deffloat,
		defsymbol,
		dollar,
		dollsym,
		gimme,
		cant,

		const Tuple = std.meta.Tuple;
		pub fn tuple(
			comptime args: []const Type,
		) Tuple(&[_]type {c_uint} ** (args.len + 1)) {
			var arr: Tuple(&[_]type {c_uint} ** (args.len + 1)) = undefined;
			inline for (0..args.len) |i| {
				arr[i] = @intFromEnum(args[i]);
			}
			arr[args.len] = @intFromEnum(Type.none);
			return arr;
		}
	};

	pub inline fn getFloat(self: *const Atom) ?Float {
		return if (self.type == .float) self.w.float else null;
	}

	pub inline fn getSymbol(self: *const Atom) ?*Symbol {
		return if (self.type == .symbol) self.w.symbol else null;
	}

	pub const toSymbol = atom_gensym;
	extern fn atom_gensym(*const Atom) *Symbol;

	pub fn bufPrint(self: *const Atom, buf: []u8) void {
		atom_string(self, buf.ptr, @intCast(buf.len));
	}
	extern fn atom_string(*const Atom, [*]u8, c_uint) void;

	pub inline fn float(f: Float) Atom {
		return .{ .type = .float, .w = .{ .float = f } };
	}

	pub inline fn symbol(s: *Symbol) Atom {
		return .{ .type = .symbol, .w = .{ .symbol = s } };
	}

	pub inline fn pointer(p: *GPointer) Atom {
		return .{ .type = .pointer, .w = .{ .gpointer = p } };
	}
};

fn typesFromAtoms(comptime args: []const Atom.Type) [args.len]type {
	var types: [args.len]type = undefined;
	for (args, 0..) |a, i| {
		types[i] = switch (a) {
			.symbol, .defsymbol => *Symbol,
			else => Float,
		};
	}
	return types;
}

fn paramsFromTypes(comptime types: []const type) [types.len]std.builtin.Type.Fn.Param {
	var params: [types.len]std.builtin.Type.Fn.Param = undefined;
	for (types, 0..) |t, i| {
		params[i] = .{
			.is_generic = false,
			.is_noalias = false,
			.type = t,
		};
	}
	return params;
}

pub fn NewFn(T: type, comptime args: []const Atom.Type) type {
	return @Type(.{ .@"fn" = std.builtin.Type.Fn{
		.calling_convention = .c,
		.is_generic = false,
		.is_var_args = false,
		.return_type = ?*T,
		.params = if (args.len == 0) &.{} else &paramsFromTypes(
			if (args[0] == .gimme)
				&.{ *Symbol, c_uint, [*]Atom }
			else
				&typesFromAtoms(args)
		),
	}});
}

pub fn addCreator(
	T: type,
	name: [:0]const u8,
	comptime args: []const Atom.Type,
	new_method: ?*const NewFn(T, args),
) void {
	const sym: *Symbol = .gen(name);
	const newm: NewMethod = @ptrCast(new_method);
	@call(.auto, class_addcreator, .{ newm, sym } ++ Atom.Type.tuple(args));
}
extern fn class_addcreator(NewMethod, *Symbol, c_uint, ...) void;

pub inline fn floatArg(idx: usize, av: []const Atom) ?Float {
	return if (av.len > idx and av[idx].type == .float) av[idx].w.float else null;
}

pub inline fn symbolArg(idx: usize, av: []const Atom) ?*Symbol {
	return if (av.len > idx and av[idx].type == .symbol) av[idx].w.symbol else null;
}


// ---------------------------------- BinBuf -----------------------------------
// -----------------------------------------------------------------------------
pub const BinBuf = opaque {
	pub const Error = error {
		BinBufDuplicate,
		BinBufFromText,
		BinBufAdd,
		BinBufJoin,
		BinBufAddSemi,
		BinBufRestore,
		BinBufRead,
		BinBufReadViaCanvas,
		BinBufWrite,
		BinBufResize,
		BinBufNew,
		BinBufFromName,
	};

	pub const free = binbuf_free;
	extern fn binbuf_free(*BinBuf) void;

	pub fn duplicate(self: *const BinBuf) Error!*BinBuf {
		return binbuf_duplicate(self) orelse Error.BinBufDuplicate;
	}
	extern fn binbuf_duplicate(*const BinBuf) ?*BinBuf;

	pub const len = binbuf_getnatom;
	extern fn binbuf_getnatom(*const BinBuf) c_uint;

	pub fn vec(self: *BinBuf) []Atom {
		return binbuf_getvec(self)[0..binbuf_getnatom(self)];
	}
	extern fn binbuf_getvec(*const BinBuf) [*]Atom;

	pub fn fromText(self: *BinBuf, txt: []const u8) Error!*void {
		binbuf_text(self, txt.ptr, txt.len);
		if (binbuf_getnatom(self) == 0)
			return Error.BinBufFromText;
	}
	extern fn binbuf_text(*BinBuf, [*]const u8, usize) void;

	/// Convert a binbuf to text. No null termination.
	pub fn text(self: *const BinBuf) []u8 {
		var ptr: [*]u8 = undefined;
		var n: c_uint = undefined;
		binbuf_gettext(self, &ptr, &n);
		return ptr[0..n];
	}
	extern fn binbuf_gettext(*const BinBuf, *[*]u8, *c_uint) void;

	pub const clear = binbuf_clear;
	extern fn binbuf_clear(*BinBuf) void;

	pub fn add(self: *BinBuf, av: []const Atom) Error!void {
		const newsize = binbuf_getnatom(self) + av.len;
		binbuf_add(self, @intCast(av.len), av.ptr);
		if (binbuf_getnatom(self) != newsize)
			return Error.BinBufAdd;
	}
	extern fn binbuf_add(*BinBuf, c_uint, [*]const Atom) void;

	pub fn addV(self: *BinBuf, fmt: [*:0]const u8, args: anytype) void {
		@call(.auto, binbuf_addv, .{ self, fmt } ++ args);
	}
	extern fn binbuf_addv(*BinBuf, fmt: [*:0]const u8, ...) void;

	/// add a binbuf to another one for saving.  Semicolons and commas go to
	/// symbols ";", "'",; and inside symbols, characters ';', ',' and '$' get
	/// escaped.  LATER also figure out about escaping white space
	pub fn join(self: *BinBuf, other: *const BinBuf) Error!void {
		const newsize = binbuf_getnatom(self) + binbuf_getnatom(other);
		binbuf_addbinbuf(self, other);
		if (binbuf_getnatom(self) != newsize)
			return Error.BinBufJoin;
	}
	extern fn binbuf_addbinbuf(*BinBuf, *const BinBuf) void;

	pub fn addSemi(self: *BinBuf) Error!void {
		const newsize = binbuf_getnatom(self) + 1;
		binbuf_addsemi(self);
		if (binbuf_getnatom(self) != newsize)
			return Error.BinBufAddSemi;
	}
	extern fn binbuf_addsemi(*BinBuf) void;

	/// Supply atoms to a binbuf from a message, making the opposite changes
	/// from `join`.  The symbol ";" goes to a semicolon, etc.
	pub fn restore(self: *BinBuf, av: []Atom) Error!void {
		const newsize = binbuf_getnatom(self) + av.len;
		binbuf_restore(self, av.len, av.ptr);
		if (binbuf_getnatom(self) != newsize)
			return Error.BinBufRestore;
	}
	extern fn binbuf_restore(*BinBuf, c_int, [*]const Atom) void;

	pub const print = binbuf_print;
	extern fn binbuf_print(*const BinBuf) void;

	pub fn eval(self: *const BinBuf, target: *Pd, av: []Atom) void {
		binbuf_eval(self, target, @intCast(av.len), av.ptr);
	}
	extern fn binbuf_eval(*const BinBuf, *Pd, c_uint, [*]const Atom) void;

	pub fn read(
		self: *BinBuf,
		filename: [*:0]const u8,
		dirname: [*:0]const u8,
		crflag: bool,
	) Error!void {
		if (binbuf_read(self, filename, dirname, @intFromBool(crflag)) != 0)
			return Error.BinBufRead;
	}
	extern fn binbuf_read(*BinBuf, [*:0]const u8, [*:0]const u8, c_uint) c_int;

	pub fn readViaCanvas(
		self: *BinBuf,
		filename: [*:0]const u8,
		canvas: *const cnv.GList,
		crflag: bool,
	) Error!void {
		if (binbuf_read_via_canvas(self, filename, canvas, @intFromBool(crflag)) != 0)
			return Error.BinBufReadViaCanvas;
	}
	extern fn binbuf_read_via_canvas(
		*BinBuf, [*:0]const u8, *const cnv.GList, c_uint) c_int;

	pub fn write(
		self: *BinBuf,
		filename: [*:0]const u8,
		dirname: [*:0]const u8,
		crflag: bool,
	) Error!void {
		if (binbuf_write(self, filename, dirname, @intFromBool(crflag)) != 0)
			return Error.BinBufWrite;
	}
	extern fn binbuf_write(*const BinBuf, [*:0]const u8, [*:0]const u8, c_uint) c_int;

	pub fn resize(self: *BinBuf, newsize: c_uint) Error!void {
		if (binbuf_resize(self, newsize) == 0)
			return Error.BinBufResize;
	}
	extern fn binbuf_resize(*BinBuf, c_uint) c_uint;

	pub fn new() Error!*BinBuf {
		return binbuf_new() orelse Error.BinBufNew;
	}
	extern fn binbuf_new() ?*BinBuf;

	/// Public interface to get text buffers by name
	pub fn fromName(sym: *Symbol) Error!*BinBuf {
		return text_getbufbyname(sym) orelse Error.BinBufFromName;
	}
	extern fn text_getbufbyname(*BinBuf) ?*BinBuf;
};

pub const evalFile = binbuf_evalfile;
extern fn binbuf_evalfile(name: *Symbol, dir: *Symbol) void;

pub fn realizeDollSym(
	sym: *Symbol,
	av: []const Atom,
	tonew: bool
) error{RealizeDollSym}!*Symbol {
	return binbuf_realizedollsym(sym, @intCast(av.len), av.ptr, @intFromBool(tonew))
		orelse error.RealizeDollSym;
}
extern fn binbuf_realizedollsym(*Symbol, c_uint, [*]const Atom, c_uint) ?*Symbol;


// ----------------------------------- Clock -----------------------------------
// -----------------------------------------------------------------------------
pub const Clock = opaque {
	pub const Error = error {
		ClockNew,
	};

	pub const free = clock_free;
	extern fn clock_free(*Clock) void;

	pub const set = clock_set;
	extern fn clock_set(*Clock, systime: f64) void;

	pub const delay = clock_delay;
	extern fn clock_delay(*Clock, delaytime: f64) void;

	pub const unset = clock_unset;
	extern fn clock_unset(*Clock) void;

	pub fn setUnit(self: *Clock, timeunit: f64, in_samples: bool) void {
		clock_setunit(self, timeunit, @intFromBool(in_samples));
	}
	extern fn clock_setunit(*Clock, f64, c_uint) void;

	pub fn new(owner: *anyopaque, func: Method) Error!*Clock {
		return clock_new(owner, func) orelse Error.ClockNew;
	}
	extern fn clock_new(*anyopaque, Method) ?*Clock;
};

pub const time = clock_getlogicaltime;
extern fn clock_getlogicaltime() f64;

pub const timeSince = clock_gettimesince;
extern fn clock_gettimesince(prevsystime: f64) f64;

pub const sysTimeAfter = clock_getsystimeafter;
extern fn clock_getsystimeafter(delaytime: f64) f64;

pub fn timeSinceWithUnits(prevsystime: f64, units: f64, in_samples: bool) f64 {
	return clock_gettimesincewithunits(prevsystime, units, @intFromBool(in_samples));
}
extern fn clock_gettimesincewithunits(f64, f64, c_uint) f64;


// ------------------------------------ Dsp ------------------------------------
// -----------------------------------------------------------------------------
pub const dsp = struct {
	pub const PerfRoutine = ?*const fn ([*]usize) callconv(.c) *usize;

	pub fn add(perf: PerfRoutine, args: anytype) void {
		@call(.auto, dsp_add, .{ perf, @as(c_uint, @intCast(args.len)) } ++ args);
	}
	extern fn dsp_add(PerfRoutine, c_uint, ...) void;

	pub fn addVec(perf: PerfRoutine, vec: []usize) void {
		dsp_addv(perf, @intCast(vec.len), vec.ptr);
	}
	extern fn dsp_addv(PerfRoutine, c_uint, [*]usize) void;

	pub const addPlus = dsp_add_plus;
	extern fn dsp_add_plus(
		in1: [*]Sample,
		in2: [*]Sample,
		out: [*]Sample,
		n: c_uint
	) void;

	pub const addCopy = dsp_add_copy;
	extern fn dsp_add_copy(in: [*]Sample, out: [*]Sample, n: c_uint) void;

	pub const addScalarCopy = dsp_add_scalarcopy;
	extern fn dsp_add_scalarcopy(in: [*]Float, out: [*]Sample, n: c_uint) void;

	pub const addZero = dsp_add_zero;
	extern fn dsp_add_zero([*]Sample, c_uint) void;
};


// ---------------------------------- GArray -----------------------------------
// -----------------------------------------------------------------------------
pub extern const garray_class: *Class;
pub extern const scalar_class: *Class;

pub const GArray = opaque {
	pub const Error = error {
		GArrayGetArray,
		GArrayFloatWords,
	};

	pub const redraw = garray_redraw;
	extern fn garray_redraw(*GArray) void;

	pub fn array(self: *GArray) Error!*cnv.Array {
		return garray_getarray(self) orelse Error.GArrayGetArray;
	}
	extern fn garray_getarray(*GArray) ?*cnv.Array;

	pub fn vec(self: *GArray) ![]u8 {
		const arr = try self.array();
		return arr.vec[0..arr.len];
	}

	pub const resize = garray_resize_long;
	extern fn garray_resize_long(*GArray, c_ulong) void;

	pub const useInDsp = garray_usedindsp;
	extern fn garray_usedindsp(*GArray) void;

	pub fn setSaveInPatch(self: *GArray, saveit: bool) void {
		garray_setsaveit(self, @intFromBool(saveit));
	}
	extern fn garray_setsaveit(*GArray, c_uint) void;

	pub const glist = garray_getglist;
	extern fn garray_getglist(*GArray) *cnv.GList;

	pub fn floatWords(self: *GArray) Error![]Word {
		var len: c_uint = undefined;
		var ptr: [*]Word = undefined;
		return if (garray_getfloatwords(self, &len, &ptr) != 0)
			ptr[0..len] else Error.GArrayFloatWords;
	}
	extern fn garray_getfloatwords(*GArray, *c_uint, vec: *[*]Word) c_uint;
};


// --------------------------------- GPointer ----------------------------------
// -----------------------------------------------------------------------------
pub const Scalar = extern struct {
	/// header for graphical object
	gobj: GObj,
	/// template name (LATER replace with pointer)
	template: *Symbol,
	/// indeterminate-length array of words
	vec: [1]Word,
};

pub const GStub = extern struct {
	un: extern union {
		glist: *cnv.GList,
		array: *cnv.Array,
	},
	which: Type,
	refcount: c_int,

	const Type = enum(c_uint) {
		none,
		glist,
		array,
	};
};

pub const GPointer = extern struct {
	un: extern union {
		scalar: *Scalar,
		w: *Word,
	},
	valid: c_int,
	stub: *GStub,

	pub const init = gpointer_init;
	extern fn gpointer_init(*GPointer) void;

	/// Copy a pointer to another, assuming the second one hasn't yet been
	/// initialized.  New gpointers should be initialized either by this
	/// routine or by gpointer_init below.
	pub const copyTo = gpointer_copy;
	extern fn gpointer_copy(from: *const GPointer, to: *GPointer) void;

	/// Clear a gpointer that was previously set, releasing the associated
	/// gstub if this was the last reference to it.
	pub const unset = gpointer_unset;
	extern fn gpointer_unset(*GPointer) void;

	/// Call this to verify that a pointer is fresh, i.e., that it either
	/// points to real data or to the head of a list, and that in either case
	/// the object hasn't disappeared since this pointer was generated.
	/// Unless "headok" is set,  the routine also fails for the head of a list.
	pub fn isValid(self: *GPointer, headok: bool) bool {
		return (gpointer_check(self, @intFromBool(headok)) != 0);
	}
	extern fn gpointer_check(*const GPointer, headok: c_uint) c_uint;
};


// ----------------------------------- Inlet -----------------------------------
// -----------------------------------------------------------------------------
pub const Inlet = opaque {
	pub const Error = error {
		InletNew,
		InletNewFloat,
		InletNewSymbol,
		InletNewSignal,
		InletNewPointer,
	};

	pub const free = inlet_free;
	extern fn inlet_free(*Inlet) void;

	pub fn new(obj: *Object, dest: *Pd, from: ?*Symbol, to: ?*Symbol) Error!*Inlet {
		return inlet_new(obj, dest, from, to) orelse Error.InletNew;
	}
	extern fn inlet_new(*Object, *Pd, ?*Symbol, ?*Symbol) ?*Inlet;

	pub fn float(obj: *Object, fp: *Float) Error!*Inlet {
		return floatinlet_new(obj, fp) orelse Error.InletNewFloat;
	}
	extern fn floatinlet_new(*Object, *Float) ?*Inlet;

	pub fn symbol(obj: *Object, sym: **Symbol) Error!*Inlet {
		return symbolinlet_new(obj, sym) orelse Error.InletNewSymbol;
	}
	extern fn symbolinlet_new(*Object, **Symbol) ?*Inlet;

	pub fn signal(obj: *Object, f: Float) Error!*Inlet {
		return signalinlet_new(obj, f) orelse Error.InletNewSignal;
	}
	extern fn signalinlet_new(*Object, Float) ?*Inlet;

	pub fn pointer(obj: *Object, gp: *GPointer) Error!*Inlet {
		return pointerinlet_new(obj, gp) orelse Error.InletNewPointer;
	}
	extern fn pointerinlet_new(*Object, *GPointer) ?*Inlet;
};


// ---------------------------------- Memory -----------------------------------
// -----------------------------------------------------------------------------
const Allocator = std.mem.Allocator;

const PdAllocator = struct {
	const builtin = @import("builtin");
	const Alignment = std.mem.Alignment;
	extern fn getbytes(usize) ?*anyopaque;
	extern fn freebytes(*anyopaque, usize) void;

	const vtable: Allocator.VTable = .{
		.alloc = alloc,
		.resize = resize,
		.remap = remap,
		.free = free,
	};

	const malloc_size = if (@TypeOf(std.c.malloc_size) != void) std.c.malloc_size
	else if (@TypeOf(std.c.malloc_usable_size) != void) std.c.malloc_usable_size
	else if (@TypeOf(std.c._msize) != void) std.c._msize
	else {};

	const supports_posix_memalign = switch (builtin.os.tag) {
		.dragonfly, .netbsd, .freebsd, .solaris, .openbsd, .linux,
		.macos, .ios, .tvos, .watchos, .visionos => true,
		else => false,
	};

	fn getHeader(ptr: [*]u8) *[*]u8 {
		return @alignCast(@ptrCast(ptr - @sizeOf(usize)));
	}

	fn alignedAlloc(len: usize, alignment: Alignment) ?[*]u8 {
		const alignment_bytes = alignment.toByteUnits();
		if (supports_posix_memalign) {
			// The posix_memalign only accepts alignment values that are a
			// multiple of the pointer size
			const effective_alignment = @max(alignment_bytes, @sizeOf(usize));

			var aligned_ptr: ?*anyopaque = undefined;
			if (std.c.posix_memalign(&aligned_ptr, effective_alignment, len) != 0)
				return null;

			return @ptrCast(aligned_ptr);
		}

		// Thin wrapper around regular malloc, overallocate to account for
		// alignment padding and store the original malloc()'ed pointer before
		// the aligned address.
		const ptr = getbytes(len + alignment_bytes - 1 + @sizeOf(usize));
		const unaligned_ptr = @as([*]u8, @ptrCast(ptr orelse return null));
		const unaligned_addr = @intFromPtr(unaligned_ptr);
		const aligned_addr = std.mem.alignForward(usize,
			unaligned_addr + @sizeOf(usize), alignment_bytes);
		const aligned_ptr = unaligned_ptr + (aligned_addr - unaligned_addr);
		getHeader(aligned_ptr).* = unaligned_ptr;

		return aligned_ptr;
	}

	fn alloc(
		_: *anyopaque,
		len: usize,
		alignment: Alignment,
		return_address: usize,
	) ?[*]u8 {
		_ = return_address;
		std.debug.assert(len > 0);
		return alignedAlloc(len, alignment);
	}

	fn alignedAllocSize(ptr: [*]u8) usize {
		if (supports_posix_memalign) {
			return malloc_size(ptr);
		}

		const unaligned_ptr = getHeader(ptr).*;
		const delta = @intFromPtr(ptr) - @intFromPtr(unaligned_ptr);
		return malloc_size(unaligned_ptr) - delta;
	}

	fn resize(
		_: *anyopaque,
		buf: []u8,
		alignment: Alignment,
		new_len: usize,
		return_address: usize,
	) bool {
		_ = alignment;
		_ = return_address;
		return new_len <= buf.len
			or (@TypeOf(malloc_size) != void and new_len <= alignedAllocSize(buf.ptr));
	}

	fn remap(
		context: *anyopaque,
		memory: []u8,
		alignment: Alignment,
		new_len: usize,
		return_address: usize,
	) ?[*]u8 {
		// realloc would potentially return a new allocation that does not
		// respect the original alignment.
		return if (resize(context, memory, alignment, new_len, return_address))
			memory.ptr else null;
	}

	fn free(
		_: *anyopaque,
		buf: []u8,
		alignment: Alignment,
		return_address: usize,
	) void {
		_ = alignment;
		_ = return_address;
		if (supports_posix_memalign) {
			return freebytes(buf.ptr, buf.len);
		}

		const unaligned_ptr = getHeader(buf.ptr).*;
		freebytes(unaligned_ptr, buf.len);
	}
};

pub const mem = Allocator{
	.ptr = undefined,
	.vtable = &PdAllocator.vtable,
};


// ---------------------------------- Object -----------------------------------
// -----------------------------------------------------------------------------
pub const GObj = extern struct {
	pd: Pd,
	next: ?*GObj,
};

pub const Object = extern struct {
	/// header for graphical object
	g: GObj,
	/// holder for the text
	binbuf: *BinBuf,
	/// linked list of outlets
	outlets: ?*Outlet,
	/// linked list of inlets
	inlets: ?*Inlet,
	/// x location (within the toplevel)
	xpix: i16,
	/// y location (within the toplevel)
	ypix: i16,
	/// requested width in chars, 0 if auto
	width: i16,
	type: Type,

	const Type = enum(u8) {
		/// just a textual comment
		text,
		/// a MAX style patchable object
		object,
		/// a MAX type message
		message,
		/// a cell to display a number or symbol
		atom,
	};

	pub fn list(self: *Object, sym: *Symbol, av: []Atom) void {
		obj_list(self, sym, av.len, av.ptr);
	}
	extern fn obj_list(*Object, *Symbol, ac: c_uint, av: [*]Atom) void;

	pub const saveFormat = obj_saveformat;
	extern fn obj_saveformat(*const Object, *BinBuf) void;

	pub const xPix = text_xpix;
	extern fn text_xpix(*Object, *cnv.GList) c_int;

	pub const yPix = text_ypix;
	extern fn text_ypix(*Object, *cnv.GList) c_int;

	pub const outlet = Outlet.new;
	pub const inlet = Inlet.new;
	pub const inletFloat = Inlet.float;
	pub const inletSymbol = Inlet.symbol;
	pub const inletSignal = Inlet.signal;
	pub const inletPointer = Inlet.pointer;

	pub inline fn inletFloatArg(
		self: *Object,
		fp: *Float,
		index: usize,
		av: []const Atom,
		fallback: Float,
	) Inlet.Error!*Inlet {
		fp.* = floatArg(index, av) orelse fallback;
		return .float(self, fp);
	}

	pub inline fn inletSymbolArg(
		self: *Object,
		sp: **Symbol,
		index: usize,
		av: []const Atom,
		fallback: *Symbol,
	) Inlet.Error!*Inlet {
		sp.* = symbolArg(index, av) orelse fallback;
		return .symbol(self, sp);
	}
};


// ---------------------------------- Outlet -----------------------------------
// -----------------------------------------------------------------------------
pub const Outlet = opaque {
	pub const Error = error {
		OutletNew,
	};

	pub const free = outlet_free;
	extern fn outlet_free(*Outlet) void;

	pub const bang = outlet_bang;
	extern fn outlet_bang(*Outlet) void;

	pub const pointer = outlet_pointer;
	extern fn outlet_pointer(*Outlet, *GPointer) void;

	pub const float = outlet_float;
	extern fn outlet_float(*Outlet, Float) void;

	pub const symbol = outlet_symbol;
	extern fn outlet_symbol(*Outlet, *Symbol) void;

	pub fn list(self: *Outlet, sym: ?*Symbol, av: []Atom) void {
		outlet_list(self, sym, @intCast(av.len), av.ptr);
	}
	extern fn outlet_list(*Outlet, ?*Symbol, c_uint, [*]Atom) void;

	pub fn anything(self: *Outlet, sym: *Symbol, av: []Atom) void {
		outlet_anything(self, sym, @intCast(av.len), av.ptr);
	}
	extern fn outlet_anything(*Outlet, *Symbol, c_uint, [*]Atom) void;

	/// Get the outlet's declared symbol
	pub const getSymbol = outlet_getsymbol;
	extern fn outlet_getsymbol(*Outlet) *Symbol;

	pub fn new(obj: *Object, atype: ?*Symbol) Error!*Outlet {
		return outlet_new(obj, atype) orelse Error.OutletNew;
	}
	extern fn outlet_new(*Object, ?*Symbol) ?*Outlet;
};


// ------------------------------------ Pd -------------------------------------
// -----------------------------------------------------------------------------
/// object to send "pd" messages
pub extern const glob_pdobject: *Class;

pub const Pd = extern struct {
	_: *const imp.Class,

	pub const free = pd_free;
	extern fn pd_free(*Pd) void;

	pub const bind = pd_bind;
	extern fn pd_bind(*Pd, *Symbol) void;

	pub const unbind = pd_unbind;
	extern fn pd_unbind(*Pd, *Symbol) void;

	pub const pushSymbol = pd_pushsym;
	extern fn pd_pushsym(*Pd) void;

	pub const popSymbol = pd_popsym;
	extern fn pd_popsym(*Pd) void;

	pub const bang = pd_bang;
	extern fn pd_bang(*Pd) void;

	pub const pointer = pd_pointer;
	extern fn pd_pointer(*Pd, *GPointer) void;

	pub const float = pd_float;
	extern fn pd_float(*Pd, Float) void;

	pub const symbol = pd_symbol;
	extern fn pd_symbol(*Pd, *Symbol) void;

	pub fn list(self: *Pd, sym: ?*Symbol, av: []Atom) void {
		pd_list(self, sym, @intCast(av.len), av.ptr);
	}
	extern fn pd_list(*Pd, ?*Symbol, c_uint, [*]Atom) void;

	pub fn anything(self: *Pd, sym: *Symbol, av: []Atom) void {
		pd_anything(self, sym, @intCast(av.len), av.ptr);
	}
	extern fn pd_anything(*Pd, *Symbol, c_uint, [*]Atom) void;

	pub fn typedMess(self: *Pd, sym: ?*Symbol, av: []Atom) void {
		pd_typedmess(self, sym, @intCast(av.len), av.ptr);
	}
	extern fn pd_typedmess(*Pd, ?*Symbol, c_uint, [*]Atom) void;

	pub fn forwardMess(self: *Pd, av: []Atom) void {
		pd_forwardmess(self, @intCast(av.len), av.ptr);
	}
	extern fn pd_forwardmess(*Pd, c_uint, [*]Atom) void;

	/// Checks that a pd is indeed a patchable object, and returns
	/// it, correctly typed, or null if the check failed.
	pub const checkObject = pd_checkobject;
	extern fn pd_checkobject(*Pd) ?*Object;

	pub const parentWidget = pd_getparentwidget;
	extern fn pd_getparentwidget(*Pd) ?*const cnv.parent.WidgetBehavior;

	pub fn stub(
		self: *Pd,
		dest: [*:0]const u8,
		key: *anyopaque,
		fmt: [*:0]const u8,
		args: anytype
	) void {
		@call(.auto, pdgui_stub_vnew, .{ self, dest, key, fmt } ++ args);
	}
	extern fn pdgui_stub_vnew(*Pd, [*:0]const u8, *anyopaque, [*:0]const u8, ...) void;

	pub const func = getfn;
	extern fn getfn(*const Pd, *Symbol) GotFn;

	pub const zFunc = zgetfn;
	extern fn zgetfn(*const Pd, *Symbol) GotFn;

	/// This is externally available, but note that it might later disappear; the
	/// whole "newest" thing is a hack which needs to be redesigned.
	pub const newest = pd_newest; // static
	extern fn pd_newest() *Pd;

	pub const new = imp.Class.pd;
};


// ----------------------------------- Post ------------------------------------
// -----------------------------------------------------------------------------
pub const post = struct {
	const buflen = max_string;
	var buf: [buflen:0]u8 = undefined;

	inline fn write(dest: [*:0]const u8, fmt: [*:0]const u8, args: anytype) void {
		if (this().stuff.printhook) |print| {
			print(&buf);
		} else if (stf.sys_printtostderr != 0 or !stf.haveTkProc()) {
			std.debug.print("{s}", .{ &buf });
		} else {
			vMess(dest, fmt, args);
		}
	}

	inline fn dopost(comptime fmt: []const u8, args: anytype) void {
		var fbs = std.io.fixedBufferStream(&buf);
		fbs.writer().any().print(fmt, args) catch {
			@memcpy(buf[buflen - 3..], "..\n");
			fbs.pos = buflen;
		};
		buf[fbs.pos] = 0;
		write("::pdwindow::post", "s", .{ &buf });
	}

	pub fn do(comptime fmt: []const u8, args: anytype) void {
		dopost(fmt ++ "\n", args);
	}

	pub fn start(comptime fmt: []const u8, args: anytype) void {
		dopost(fmt, args);
	}

	pub fn end() void {
		dopost("\n", .{});
	}

	pub const string = poststring;
	extern fn poststring([*:0]const u8) void;

	pub const float = postfloat;
	extern fn postfloat(f: Float) void;

	pub fn atom(av: []const Atom) void {
		postatom(av.len, av.ptr);
	}
	extern fn postatom(c_uint, [*]const Atom) void;

	pub const LogLevel = enum(c_uint) {
		critical,
		err,
		normal,
		debug,
		verbose,
		_,
	};

	pub fn log(
		object: ?*const anyopaque,
		level: LogLevel,
		comptime fmt: []const u8,
		args: anytype,
	) void {
		const i: c_uint = @intFromEnum(level);
		var fbs = std.io.fixedBufferStream(&buf);
		const stream = fbs.writer().any();
		switch (level) {
			.critical, .normal, .debug => {},
			.err => stream.writeAll("error: ") catch unreachable,
			else => {
				if (stf.sys_verbose == 0) {
					return;
				} else {
					stream.print("verbose({}): ", .{ i }) catch unreachable;
				}
			},
		}
		const prefix = fbs.pos;
		stream.print(fmt ++ "\n", args) catch {
			@memcpy(buf[buflen - 3..], "..\n");
			fbs.pos = buflen;
		};
		buf[fbs.pos] = 0;
		write("::pdwindow::logpost", "ois", .{ object, i, buf[prefix..].ptr });
	}

	pub fn err(obj: ?*const anyopaque, comptime fmt: []const u8, args: anytype) void {
		log(obj, .err, fmt, args);
	}
};

pub var fmt_buf: [max_string:0]u8 = undefined;
const gmin = @exp(@log(10.0) * -4);
const gmax = @exp(@log(10.0) * 6);

pub fn writeG(stream: std.io.AnyWriter, f: Float) !void {
	const g = @abs(f);
	if (g != 0 and (g < gmin or gmax <= g)) {
		try stream.print("{e}", .{ f });
	} else {
		try stream.print("{d}", .{ f });
	}
}

/// Good for when you only need to format 1 number. Otherwise, use `writeG()`
pub fn fmtG(f: Float) []const u8 {
	var fbs = std.io.fixedBufferStream(&fmt_buf);
	writeG(fbs.writer().any(), f) catch return "?";
	return fbs.getWritten();
}


// --------------------------------- Resample ----------------------------------
// -----------------------------------------------------------------------------
pub const Resample = extern struct {
	const Converter = enum(c_uint) {
		zero_padding = 0,
		zero_order_hold = 1,
		linear = 2,
	};

	/// unused
	method: Converter,

	/// downsampling factor
	downsample: c_uint,
	/// upsampling factor
	upsample: c_uint,

	/// here we hold the resampled data
	vec: [*]Sample,
	n: c_uint,

	/// coefficients for filtering...
	coeffs: [*]Sample,
	coef_size: c_uint,

	/// buffer for filtering
	buffer: [*]Sample,
	buf_size: c_uint,

	pub const free = resample_free;
	extern fn resample_free(*Resample) void;

	pub const init = resample_init;
	extern fn resample_init(*Resample) void;

	pub fn dsp(self: *Resample, in: []Sample, out: []Sample, conv: Converter) void {
		resample_dsp(self, in.ptr, @intCast(in.len), out.ptr, @intCast(out.len), conv);
	}
	extern fn resample_dsp(*Resample, *Sample, c_uint, *Sample, c_uint, Converter) void;

	pub fn dspFrom(self: *Resample, in: []Sample, out_len: usize, conv: Converter) void {
		resamplefrom_dsp(self, in.ptr, @intCast(in.len), @intCast(out_len), conv);
	}
	extern fn resamplefrom_dsp(*Resample, *Sample, c_uint, c_uint, Converter) void;

	pub fn dspTo(self: *Resample, out: []Sample, in_len: usize, conv: Converter) void {
		resampleto_dsp(self, out.ptr, @intCast(in_len), @intCast(out.len), conv);
	}
	extern fn resampleto_dsp(*Resample, *Sample, c_uint, c_uint, Converter) void;
};


// ---------------------------------- Signal -----------------------------------
// -----------------------------------------------------------------------------
pub const Signal = extern struct {
	pub const Error = error {
		SignalNew,
	};

	len: c_uint,
	vec: [*]Sample,
	srate: Float,
	nchans: c_uint,
	overlap: c_int,
	refcount: c_uint,
	isborrowed: c_uint,
	isscalar: c_uint,
	borrowedfrom: ?*Signal,
	nextfree: ?*Signal,
	nextused: ?*Signal,
	nalloc: c_uint,

	/// Pop an audio signal from free list or create a new one.
	///
	/// If `scalarptr` is nonzero, it's a pointer to a scalar owned by the
	/// tilde object. In this case, we neither allocate nor free it.
	/// Otherwise, if `length` is zero, return a "borrowed"
	/// signal whose buffer and size will be obtained later via
	/// `signal_setborrowed()`.
	pub fn new(
		length: c_uint,
		nchans: c_uint,
		samplerate: Float,
		scalarptr: *Sample
	) Error!*Signal {
		return signal_new(length, nchans, samplerate, scalarptr) orelse Error.SignalNew;
	}
	extern fn signal_new(c_uint, c_uint, Float, *Sample) ?*Signal;

	/// Only use this in the context of dsp routines to set number of channels
	/// on output signal - we assume it's currently a pointer to the null signal.
	pub const setMultiOut = signal_setmultiout;
	extern fn signal_setmultiout(**Signal, c_uint) void;
};


// ---------------------------------- Symbol -----------------------------------
// -----------------------------------------------------------------------------
pub const Symbol = extern struct {
	name: [*:0]const u8,
	thing: ?*Pd,
	next: ?*Symbol,

	pub const gen = gensym;
	extern fn gensym([*:0]const u8) *Symbol; // could run out of memory
};

pub const setExternDir = class_set_extern_dir;
extern fn class_set_extern_dir(*Symbol) void;

pub const notify = text_notifybyname;
extern fn text_notifybyname(*Symbol) void;

pub extern var s_pointer: Symbol;
pub extern var s_float: Symbol;
pub extern var s_symbol: Symbol;
pub extern var s_bang: Symbol;
pub extern var s_list: Symbol;
pub extern var s_anything: Symbol;
pub extern var s_signal: Symbol;
pub extern var s__N: Symbol;
pub extern var s__X: Symbol;
pub extern var s_x: Symbol;
pub extern var s_y: Symbol;
pub extern var s_: Symbol;


// ---------------------------------- System -----------------------------------
// -----------------------------------------------------------------------------
pub const GuiCallbackFn = ?*const fn (*GObj, *cnv.GList) callconv(.c) void;

pub const blockSize = sys_getblksize;
extern fn sys_getblksize() c_uint;

pub const sampleRate = sys_getsr;
extern fn sys_getsr() Float;

pub const inChannels = sys_get_inchannels;
extern fn sys_get_inchannels() c_uint;

pub const outChannels = sys_get_outchannels;
extern fn sys_get_outchannels() c_uint;

/// If some GUI object is having to do heavy computations, it can tell
/// us to back off from doing more updates by faking a big one itself.
pub const pretendGuiBytes = sys_pretendguibytes;
extern fn sys_pretendguibytes(c_uint) void;

pub const queueGui = sys_queuegui;
extern fn sys_queuegui(client: *anyopaque, glist: *cnv.GList, f: GuiCallbackFn) void;

pub const unqueueGui = sys_unqueuegui;
extern fn sys_unqueuegui(client: *anyopaque) void;

pub const version = sys_getversion;
extern fn sys_getversion(major: *c_uint, minor: *c_uint, bugfix: *c_uint) c_uint;

pub const floatSize = sys_getfloatsize;
extern fn sys_getfloatsize() c_uint;

/// Get "real time" in seconds. Take the
/// first time we get called as a reference time of zero.
pub const realTime = sys_getrealtime;
extern fn sys_getrealtime() f64;

pub const lock = sys_lock;
extern fn sys_lock() void;

pub const unlock = sys_unlock;
extern fn sys_unlock() void;

pub const tryLock = sys_trylock;
extern fn sys_trylock() c_int;

pub const hostFontSize = sys_hostfontsize;
extern fn sys_hostfontsize(c_uint, c_uint) c_uint;

pub fn zoomFontWidth(fontsize: c_uint, zoom: c_uint, worst_case: bool) c_uint {
	return sys_zoomfontwidth(fontsize, zoom, @intFromBool(worst_case));
}
extern fn sys_zoomfontwidth(c_uint, c_uint, c_uint) c_uint;

pub fn zoomFontHeight(fontsize: c_uint, zoom: c_uint, worst_case: bool) c_uint {
	return sys_zoomfontheight(fontsize, zoom, @intFromBool(worst_case));
}
extern fn sys_zoomfontheight(c_uint, c_uint, c_uint) c_uint;

pub const fontWidth = sys_fontwidth;
extern fn sys_fontwidth(c_uint) c_uint;

pub const fontHeight = sys_fontheight;
extern fn sys_fontheight(c_uint) c_uint;

pub fn isAbsolutePath(dir: [*:0]const u8) bool {
	return (sys_isabsolutepath(dir) != 0);
}
extern fn sys_isabsolutepath([*:0]const u8) c_uint;

pub fn currentDir() ?*Symbol {
	// avoid `extern fn canvas_getcurrentdir()`, it will cause pd to crash.
	return if (GList.current()) |glist| glist.dir() else null;
}

/// DSP can be suspended before, and resumed after, operations which
/// might affect the DSP chain.  For example, we suspend before loading and
/// resume afterward, so that DSP doesn't get resorted for every DSP object
/// in the patch.
pub fn suspendDsp() bool {
	return (canvas_suspend_dsp() != 0);
}
extern fn canvas_suspend_dsp() c_uint;

pub fn resumeDsp(old_state: bool) void {
	canvas_resume_dsp(@intFromBool(old_state));
}
extern fn canvas_resume_dsp(c_uint) void;

/// this is equivalent to suspending and resuming in one step.
pub const updateDsp = canvas_update_dsp;
extern fn canvas_update_dsp() void;

pub const setFileName = glob_setfilename;
extern fn glob_setfilename(dummy: *anyopaque, name: *Symbol, dir: *Symbol) void;

pub const canvasList = pd_getcanvaslist;
extern fn pd_getcanvaslist() *cnv.GList;

pub fn dspState() bool {
	return (pd_getdspstate() != 0);
}
extern fn pd_getdspstate() c_uint;


// ----------------------------------- Value -----------------------------------
// -----------------------------------------------------------------------------
pub const value = struct {
	pub const Error = error {
		ValueGet,
		ValueSet,
	};

	/// Get a pointer to a named floating-point variable.  The variable
	/// belongs to a `vcommon` object, which is created if necessary.
	pub const from = value_get;
	extern fn value_get(*Symbol) *Float;

	pub const release = value_release;
	extern fn value_release(*Symbol) void;

	/// obtain the float value of a "value" object
	pub fn get(sym: *Symbol, f: *Float) Error!void {
		if (value_getfloat(sym, f) != 0)
			return Error.ValueGet;
	}
	extern fn value_getfloat(*Symbol, *Float) c_int;

	pub fn set(sym: *Symbol, f: Float) Error!void {
		if (value_setfloat(sym, f) != 0)
			return Error.ValueSet;
	}
	extern fn value_setfloat(*Symbol, Float) c_int;
};


// ----------------------------------- Misc ------------------------------------
// -----------------------------------------------------------------------------
pub const object_maker = &pd_objectmaker;
pub extern var pd_objectmaker: Pd;

pub const canvas_maker = &pd_canvasmaker;
pub extern var pd_canvasmaker: Pd;

pub const GotFn = ?*const fn (*anyopaque, ...) callconv(.c) void;
pub const GotFn1 = ?*const fn (*anyopaque, *anyopaque) callconv(.c) void;
pub const GotFn2 = ?*const fn (*anyopaque, *anyopaque, *anyopaque) callconv(.c) void;
pub const GotFn3 = ?*const fn (*anyopaque, *anyopaque, *anyopaque, *anyopaque) callconv(.c) void;
pub const GotFn4 = ?*const fn (*anyopaque, *anyopaque, *anyopaque, *anyopaque, *anyopaque) callconv(.c) void;
pub const GotFn5 = ?*const fn (*anyopaque, *anyopaque, *anyopaque, *anyopaque, *anyopaque, *anyopaque) callconv(.c) void;

pub const nullFn = nullfn;
extern fn nullfn() void;

pub const font: [*:0]u8 = @extern([*:0]u8, .{ .name = "sys_font" });
pub const font_weight: [*:0]u8 = @extern([*:0]u8, .{ .name = "sys_fontweight" });

/// Get a number unique to the (clock, MIDI, GUI, etc.) event we're on
pub const eventNumber = sched_geteventno;
extern fn sched_geteventno() c_uint;

/// sys_idlehook is a hook the user can fill in to grab idle time.  Return
/// nonzero if you actually used the time; otherwise we're really really idle and
/// will now sleep.
pub extern var sys_idlehook: ?*const fn () callconv(.c) c_int;

pub const plusPerform = plus_perform;
extern fn plus_perform(args: [*]usize) *usize;

pub const plusPerf8 = plus_perf8;
extern fn plus_perf8(args: [*]usize) *usize;

pub const zeroPerform = zero_perform;
extern fn zero_perform(args: [*]usize) *usize;

pub const zeroPerf8 = zero_perf8;
extern fn zero_perf8(args: [*]usize) *usize;

pub const copyPerform = copy_perform;
extern fn copy_perform(args: [*]usize) *usize;

pub const copyPerf8 = copy_perf8;
extern fn copy_perf8(args: [*]usize) *usize;

pub const scalarCopyPerform = scalarcopy_perform;
extern fn scalarcopy_perform(args: [*]usize) *usize;

pub const scalarCopyPerf8 = scalarcopy_perf8;
extern fn scalarcopy_perf8(args: [*]usize) *usize;

pub const mayer = struct {
	pub fn fht(fz: []Sample) void {
		mayer_fht(fz.ptr, @intCast(fz.len));
	}
	extern fn mayer_fht([*]Sample, c_uint) void;

	pub const fft = mayer_fft;
	extern fn mayer_fft(c_uint, [*]Sample, [*]Sample) void;

	pub const ifft = mayer_ifft;
	extern fn mayer_ifft(c_uint, [*]Sample, [*]Sample) void;

	pub fn realfft(real: []Sample) void {
		mayer_realfft(@intCast(real.len), real.ptr);
	}
	extern fn mayer_realfft(c_uint, [*]Sample) void;

	pub fn realifft(real: []Sample) void {
		mayer_realifft(@intCast(real.len), real.ptr);
	}
	extern fn mayer_realifft(c_uint, [*]Sample) void;
};

pub fn fft(buf: []Float, inverse: bool) void {
	pd_fft(buf.ptr, @intCast(buf.len), @intFromBool(inverse));
}
extern fn pd_fft([*]Float, c_uint, c_uint) void;

const ushift = std.meta.Int(.unsigned, @log2(@as(Float, @bitSizeOf(usize))));
pub fn ulog2(n: usize) ushift {
	var i = n;
	var r: ushift = 0;
	while (i > 1) : (i >>= 1) {
		r += 1;
	}
	return r;
}

pub const mToF = mtof;
extern fn mtof(Float) Float;

pub const fToM = ftom;
extern fn ftom(Float) Float;

pub const rmsToDb = rmstodb;
extern fn rmstodb(Float) Float;

pub const powToDb = powtodb;
extern fn powtodb(Float) Float;

pub const dbToRms = dbtorms;
extern fn dbtorms(Float) Float;

pub const dbToPow = dbtopow;
extern fn dbtopow(Float) Float;

pub const q8Sqrt = q8_sqrt;
extern fn q8_sqrt(Float) Float;

pub const q8Rsqrt = q8_rsqrt;
extern fn q8_rsqrt(Float) Float;

pub const qSqrt = qsqrt;
extern fn qsqrt(Float) Float;

pub const qRsqrt = qrsqrt;
extern fn qrsqrt(Float) Float;

pub fn vMess(destination: ?[*:0]const u8, fmt: ?[*:0]const u8, args: anytype) void {
	@call(.auto, pdgui_vmess, .{ destination, fmt } ++ args);
}
extern fn pdgui_vmess(?[*:0]const u8, ?[*:0]const u8, ...) void;

pub const deleteStubForKey = pdgui_stub_deleteforkey;
extern fn pdgui_stub_deleteforkey(key: *anyopaque) void;

const float_bits = @bitSizeOf(Float);
const mantissa_bits = std.math.floatMantissaBits(Float);
const exponent_bits = std.math.floatExponentBits(Float);
const exp_mask = ((1 << exponent_bits) - 1) << mantissa_bits;
const bos_mask = 1 << (float_bits - 3);

pub const BigOrSmall = extern union {
	f: Float,
	ui: std.meta.Int(.unsigned, float_bits),
};

pub fn badFloat(f: Float) bool {
	var pun = BigOrSmall{ .f = f };
	pun.ui &= exp_mask;
	return (f != 0 and (pun.ui == 0 or pun.ui == exp_mask));
}

test badFloat {
	try std.testing.expect(badFloat((BigOrSmall{ .ui = exp_mask }).f)); // infinity
	try std.testing.expect(badFloat((BigOrSmall{ .ui = exp_mask + 1 }).f)); // NaN
	try std.testing.expect(badFloat((BigOrSmall{ .ui = 1 }).f)); // denormal
	try std.testing.expect(!badFloat(123.45)); // good float
}

pub fn bigOrSmall(f: Float) bool {
	const pun = BigOrSmall{ .f = f };
	return ((pun.ui & bos_mask) == ((pun.ui >> 1) & bos_mask));
}

test bigOrSmall {
	const big = if (float_bits == 64) 0x1p513 else 0x1p65;
	const small = if (float_bits == 64) 0x1p-512 else 0x1p-64;
	const almost_big = if (float_bits == 64) 0x1p512 else 0x1p64;
	const almost_small = if (float_bits == 64) 0x1p-511 else 0x1p-63;
	try std.testing.expect(bigOrSmall(big));
	try std.testing.expect(bigOrSmall(small));
	try std.testing.expect(!bigOrSmall(almost_big));
	try std.testing.expect(!bigOrSmall(almost_small));
}

pub const Instance = extern struct {
	pub const Midi = opaque {};
	pub const Inter = opaque {};
	pub const Ugen = opaque {};

	/// global time in Pd ticks
	systime: f64,
	/// linked list of set clocks
	clock_setlist: ?*Clock,
	/// linked list of all root canvases
	canvaslist: ?*cnv.GList,
	/// linked list of all templates
	templatelist: ?*cnv.Template,
	/// ordinal number of this instance
	instanceno: c_uint,
	/// symbol table hash table
	symhash: [*]*Symbol,
	/// private stuff for x_midi.c
	midi: *Midi,
	/// private stuff for s_inter.c
	inter: *Inter,
	/// private stuff for d_ugen.c
	ugen: *Ugen,
	/// semi-private stuff in g_canvas.h
	gui: *GList.Instance,
	/// semi-private stuff in s_stuff.h
	stuff: *stf.Instance,
	/// most recently created object
	newest: *Pd,

	// islocked: c_uint, // should only exist if threads are enabled
};
pub extern const pd_maininstance: Instance;

pub fn this() *const Instance {
	// TODO: fix this to be multi-instance compatible
	return &pd_maininstance;
}

pub const max_string = 1000;
pub const max_arg = 5;
pub const max_logsig = 32;
pub const max_sigsize = 1 << max_logsig;
pub const threads = 1;
