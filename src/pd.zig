const std = @import("std");
pub const imp = @import("imp.zig");
pub const cnv = @import("canvas.zig");
pub const iem = @import("all_guis.zig");
const strlen = @cImport({ @cInclude("string.h"); }).strlen;

/// non-zero on success/true
const Bool = c_uint;
/// zero on success, otherwise represents an error
const Result = c_int;

pub const Error = BinBuf.Error || Clock.Error || GArray.Error || Object.Error
	|| Signal.Error || value.Error || error.RealizeDollSym;

pub extern const pd_compatibilitylevel: c_int;

pub const Float = std.meta.Float(@import("options").float_size);
pub const Sample = Float;

pub const Method = ?*const fn () callconv(.C) void;
pub const NewMethod = ?*const fn () callconv(.C) ?*anyopaque;

pub const Class = imp.Class;
pub const class = Class.new;

pub const Gui = iem.Gui;
pub const gui = Gui.new;

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
	const Self = @This();

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
		pub fn tuple(comptime args: []const Type)
		Tuple(&[_]type {c_uint} ** (args.len + 1)) {
			var arr: Tuple(&[_]type {c_uint} ** (args.len + 1)) = undefined;
			inline for (0..args.len) |i| {
				arr[i] = @intFromEnum(args[i]);
			}
			arr[args.len] = @intFromEnum(Type.none);
			return arr;
		}
	};

	pub const int = atom_getint;
	extern fn atom_getint(*const Self) isize;

	pub const float = atom_getfloat;
	extern fn atom_getfloat(*const Self) Float;

	pub const symbol = atom_getsymbol;
	extern fn atom_getsymbol(*const Self) *Symbol;

	pub const toSymbol = atom_gensym;
	extern fn atom_gensym(*const Self) *Symbol;

	pub fn bufPrint(self: *const Self, buf: []u8) void {
		atom_string(self, buf.ptr, @intCast(buf.len));
	}
	extern fn atom_string(*const Self, [*]u8, c_uint) void;
};

pub fn addCreator(
	newmethod: NewMethod,
	sym: *Symbol,
	comptime args: []const Atom.Type,
) void {
	@call(.auto, class_addcreator, .{ newmethod, sym } ++ Atom.Type.tuple(args));
}
extern fn class_addcreator(NewMethod, *Symbol, c_uint, ...) void;

pub fn intArg(av: []const Atom, which: usize) isize {
	return atom_getintarg(@intCast(which), @intCast(av.len), av.ptr);
}
extern fn atom_getintarg(c_uint, c_uint, [*]const Atom) isize;

pub fn floatArg(av: []const Atom, which: usize) Float {
	return atom_getfloatarg(@intCast(which), @intCast(av.len), av.ptr);
}
extern fn atom_getfloatarg(c_uint, c_uint, [*]const Atom) Float;

pub fn symbolArg(av: []const Atom, which: usize) *Symbol {
	return atom_getsymbolarg(@intCast(which), @intCast(av.len), av.ptr);
}
extern fn atom_getsymbolarg(c_uint, c_uint, [*]const Atom) *Symbol;


// ---------------------------------- BinBuf -----------------------------------
// -----------------------------------------------------------------------------
pub const BinBuf = opaque {
	const Self = @This();

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
	const Err = Self.Error;

	pub const free = binbuf_free;
	extern fn binbuf_free(*Self) void;

	pub fn duplicate(self: *const Self) Err!*Self {
		return binbuf_duplicate(self) orelse Err.BinBufDuplicate;
	}
	extern fn binbuf_duplicate(*const Self) ?*Self;

	pub fn len(self: *const Self) u32 {
		return binbuf_getnatom(self);
	}
	extern fn binbuf_getnatom(*const Self) c_uint;

	pub fn vec(self: *Self) []Atom {
		return binbuf_getvec(self)[0..binbuf_getnatom(self)];
	}
	extern fn binbuf_getvec(*const Self) [*]Atom;

	pub fn fromText(txt: [:0]const u8) Err!*Self {
		const self = try Self.new();
		errdefer self.free();
		binbuf_text(self, txt.ptr, txt.len);
		return if (binbuf_getnatom(self) == 0) Err.BinBufFromText else self;
	}
	extern fn binbuf_text(*Self, [*:0]const u8, usize) void;

	/// Convert a binbuf to text. No null termination.
	pub fn text(self: *const Self) []u8 {
		var ptr: [*]u8 = undefined;
		var n: c_uint = undefined;
		binbuf_gettext(self, &ptr, &n);
		return ptr[0..n];
	}
	extern fn binbuf_gettext(*const Self, *[*]u8, *c_uint) void;

	pub const clear = binbuf_clear;
	extern fn binbuf_clear(*Self) void;

	pub fn add(self: *Self, av: []const Atom) Err!void {
		const newsize = binbuf_getnatom(self) + av.len;
		binbuf_add(self, @intCast(av.len), av.ptr);
		if (binbuf_getnatom(self) != newsize)
			return Err.BinBufAdd;
	}
	extern fn binbuf_add(*Self, c_uint, [*]const Atom) void;

	/// add a binbuf to another one for saving.  Semicolons and commas go to
	/// symbols ";", "'",; and inside symbols, characters ';', ',' and '$' get
	/// escaped.  LATER also figure out about escaping white space
	pub fn join(self: *Self, other: *const Self) Err!void {
		const newsize = binbuf_getnatom(self) + binbuf_getnatom(other);
		binbuf_addbinbuf(self, other);
		if (binbuf_getnatom(self) != newsize)
			return Err.BinBufJoin;
	}
	extern fn binbuf_addbinbuf(*Self, *const Self) void;

	pub fn addSemi(self: *Self) Err!void {
		const newsize = binbuf_getnatom(self) + 1;
		binbuf_addsemi(self);
		if (binbuf_getnatom(self) != newsize)
			return Err.BinBufAddSemi;
	}
	extern fn binbuf_addsemi(*Self) void;

	/// Supply atoms to a binbuf from a message, making the opposite changes
	/// from `join`.  The symbol ";" goes to a semicolon, etc.
	pub fn restore(self: *Self, av: []Atom) Err!void {
		const newsize = binbuf_getnatom(self) + av.len;
		binbuf_restore(self, av.len, av.ptr);
		if (binbuf_getnatom(self) != newsize)
			return Err.BinBufRestore;
	}
	extern fn binbuf_restore(*Self, c_int, [*]const Atom) void;

	pub const print = binbuf_print;
	extern fn binbuf_print(*const Self) void;

	pub fn eval(self: *const Self, target: *Pd, av: []Atom) void {
		binbuf_eval(self, target, @intCast(av.len), av.ptr);
	}
	extern fn binbuf_eval(*const Self, *Pd, c_uint, [*]const Atom) void;

	pub fn read(
		self: *Self,
		filename: [:0]const u8,
		dirname: [:0]const u8,
		crflag: bool,
	) Err!void {
		if (binbuf_read(self, filename.ptr, dirname.ptr, @intFromBool(crflag)) != 0)
			return Err.BinBufRead;
	}
	extern fn binbuf_read(*Self, [*:0]const u8, [*:0]const u8, c_uint) Result;

	pub fn readViaCanvas(
		self: *Self,
		filename: [:0]const u8,
		canvas: *const cnv.GList,
		crflag: bool,
	) Err!void {
		if (binbuf_read_via_canvas(self, filename.ptr, canvas, @intFromBool(crflag)) != 0)
			return Err.BinBufReadViaCanvas;
	}
	extern fn binbuf_read_via_canvas(
		*Self, [*:0]const u8, *const cnv.GList, c_uint) Result;

	pub fn write(
		self: *Self,
		filename: [:0]const u8,
		dirname: [:0]const u8,
		crflag: bool,
	) Err!void {
		if (binbuf_write(self, filename.ptr, dirname.ptr, @intFromBool(crflag)) != 0)
			return Err.BinBufWrite;
	}
	extern fn binbuf_write(*const Self, [*:0]const u8, [*:0]const u8, c_uint) Result;

	pub fn resize(self: *Self, newsize: u32) Err!void {
		if (binbuf_resize(self, @intCast(newsize)) == 0)
			return Err.BinBufResize;
	}
	extern fn binbuf_resize(*Self, c_uint) Bool;

	pub fn new() Err!*Self {
		return binbuf_new() orelse Err.BinBufNew;
	}
	extern fn binbuf_new() ?*Self;

	/// Public interface to get text buffers by name
	pub fn fromName(sym: *Symbol) Err!*Self {
		return text_getbufbyname(sym) orelse Err.BinBufFromName;
	}
	extern fn text_getbufbyname(*Self) ?*BinBuf;
};

pub const binbuf = BinBuf.new;

pub const evalFile = binbuf_evalfile;
extern fn binbuf_evalfile(name: *Symbol, dir: *Symbol) void;

pub fn realizeDollSym(sym: *Symbol, av: []const Atom, tonew: bool) Error!*Symbol {
	return binbuf_realizedollsym(sym, @intCast(av.len), av.ptr, @intFromBool(tonew))
		orelse Error.RealizeDollSym;
}
extern fn binbuf_realizedollsym(*Symbol, c_uint, [*]const Atom, c_uint) ?*Symbol;


// ----------------------------------- Clock -----------------------------------
// -----------------------------------------------------------------------------
pub const Clock = opaque {
	const Self = @This();

	pub const Error = error {
		ClockNew,
	};
	const Err = Self.Error;

	pub const set = clock_set;
	extern fn clock_set(*Self, systime: f64) void;

	pub const delay = clock_delay;
	extern fn clock_delay(*Self, delaytime: f64) void;

	pub const unset = clock_unset;
	extern fn clock_unset(*Self) void;

	pub const free = clock_free;
	extern fn clock_free(*Self) void;

	pub fn setUnit(self: *Self, timeunit: f64, in_samples: bool) void {
		clock_setunit(self, timeunit, @intFromBool(in_samples));
	}
	extern fn clock_setunit(*Self, f64, c_uint) void;

	pub fn new(owner: *anyopaque, func: Method) Err!*Self {
		return clock_new(owner, func) orelse Err.ClockNew;
	}
	extern fn clock_new(*anyopaque, Method) ?*Self;
};

pub const clock = Clock.new;

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
	pub const PerfRoutine = ?*const fn ([*]usize) callconv(.C) *usize;

	pub fn add(perf: PerfRoutine, args: anytype) void {
		@call(.auto, dsp_add, .{ perf, @as(c_uint, @intCast(args.len)) } ++ args);
	}
	extern fn dsp_add(PerfRoutine, c_uint, ...) void;

	pub fn addVec(perf: PerfRoutine, vec: []usize) void {
		dsp_addv(perf, @intCast(vec.len), vec.ptr);
	}
	extern fn dsp_addv(PerfRoutine, c_uint, [*]usize) void;

	pub fn addPlus(in1: [*]Sample, in2: [*]Sample, out: [*]Sample, n: usize) void {
		dsp_add_plus(in1, in2, out, @intCast(n));
	}
	extern fn dsp_add_plus([*]Sample, [*]Sample, [*]Sample, c_uint) void;

	pub fn addCopy(in1: [*]Sample, out: [*]Sample, n: usize) void {
		dsp_add_copy(in1, out, @intCast(n));
	}
	extern fn dsp_add_copy([*]Sample, [*]Sample, c_uint) void;

	pub fn addScalarCopy(in: [*]Float, out: [*]Sample, n: usize) void {
		dsp_add_scalarcopy(in, out, @intCast(n));
	}
	extern fn dsp_add_scalarcopy(in: [*]Float, out: [*]Sample, n: c_uint) void;

	pub fn addZero(out: [*]Sample, n: usize) void {
		dsp_add_zero(out, @intCast(n));
	}
	extern fn dsp_add_zero(out: [*]Sample, n: c_uint) void;
};


// ---------------------------------- GArray -----------------------------------
// -----------------------------------------------------------------------------
pub const GArray = opaque {
	const Self = @This();

	pub const Error = error {
		GArrayGetArray,
		GArrayFloatWords,
	};
	const Err = Self.Error;

	pub const redraw = garray_redraw;
	extern fn garray_redraw(*Self) void;

	pub fn array(self: *Self) Err!*cnv.Array {
		return garray_getarray(self) orelse Err.GArrayGetArray;
	}
	extern fn garray_getarray(*Self) ?*cnv.Array;

	pub fn vec(self: *Self) ![]u8 {
		const arr = try self.array();
		return arr.vec[0..arr.len];
	}

	pub const resize = garray_resize_long;
	extern fn garray_resize_long(*Self, c_ulong) void;

	pub const useInDsp = garray_usedindsp;
	extern fn garray_usedindsp(*Self) void;

	pub fn setSaveInPatch(self: *Self, saveit: bool) void {
		garray_setsaveit(self, @intFromBool(saveit));
	}
	extern fn garray_setsaveit(*Self, c_uint) void;

	pub const glist = garray_getglist;
	extern fn garray_getglist(*Self) *cnv.GList;

	pub fn floatWords(self: *GArray) Err![]Word {
		var len: c_uint = undefined;
		var ptr: [*]Word = undefined;
		return if (garray_getfloatwords(self, &len, &ptr) != 0)
			ptr[0..len] else Err.GArrayFloatWords;
	}
	extern fn garray_getfloatwords(*Self, *c_uint, vec: *[*]Word) Bool;
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
	const Self = @This();

	un: extern union {
		scalar: *Scalar,
		w: *Word,
	},
	valid: c_int,
	stub: *GStub,

	pub const init = gpointer_init;
	extern fn gpointer_init(*Self) void;

	/// Copy a pointer to another, assuming the second one hasn't yet been
	/// initialized.  New gpointers should be initialized either by this
	/// routine or by gpointer_init below.
	pub const copyTo = gpointer_copy;
	extern fn gpointer_copy(from: *const Self, to: *Self) void;

	/// Clear a gpointer that was previously set, releasing the associated
	/// gstub if this was the last reference to it.
	pub const unset = gpointer_unset;
	extern fn gpointer_unset(*Self) void;

	/// Call this to verify that a pointer is fresh, i.e., that it either
	/// points to real data or to the head of a list, and that in either case
	/// the object hasn't disappeared since this pointer was generated.
	/// Unless "headok" is set,  the routine also fails for the head of a list.
	pub fn isValid(self: *Self, headok: bool) bool {
		return (gpointer_check(self, @intFromBool(headok)) != 0);
	}
	extern fn gpointer_check(*const Self, headok: c_int) Bool;
};


// ---------------------------------- Memory -----------------------------------
// -----------------------------------------------------------------------------
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;

fn alloc(_: *anyopaque, len: usize, _: u8, _: usize) ?[*]u8 {
	assert(len > 0);
	return @ptrCast(getbytes(len));
}
extern fn getbytes(usize) ?*anyopaque;

fn resize(_: *anyopaque, buf: []u8, _: u8, new_len: usize, _: usize) bool {
	return (new_len <= buf.len);
}

fn free(_: *anyopaque, buf: []u8, _: u8, _: usize) void {
	freebytes(buf.ptr, buf.len);
}
extern fn freebytes(*anyopaque, usize) void;

const mem_vtable = Allocator.VTable{
	.alloc = alloc,
	.resize = resize,
	.free = free,
};
pub const mem = Allocator{
	.ptr = undefined,
	.vtable = &mem_vtable,
};


// ---------------------------------- Object -----------------------------------
// -----------------------------------------------------------------------------
pub const GObj = extern struct {
	pd: Pd,
	next: ?*GObj,
};

pub const Inlet = opaque {
	pub const free = inlet_free;
	extern fn inlet_free(*Inlet) void;
};

pub const Object = extern struct {
	const Self = @This();

	pub const Error = error {
		ObjectNewOutlet,
		ObjectNewInlet,
		ObjectNewInletPointer,
		ObjectNewInletFloat,
		ObjectNewInletSymbol,
		ObjectNewInletSignal,
	};
	const Err = Self.Error;

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

	pub fn list(self: *Self, sym: *Symbol, av: []Atom) void {
		obj_list(self, sym, av.len, av.ptr);
	}
	extern fn obj_list(*Self, *Symbol, ac: c_uint, av: [*]Atom) void;

	pub const saveFormat = obj_saveformat;
	extern fn obj_saveformat(*const Self, *BinBuf) void;

	pub fn outlet(self: *Self, atype: ?*Symbol) Err!*Outlet {
		return outlet_new(self, atype) orelse Err.ObjectNewOutlet;
	}
	extern fn outlet_new(*Self, ?*Symbol) ?*Outlet;

	pub fn inlet(self: *Self, dest: *Pd, from: ?*Symbol, to: ?*Symbol) Err!*Inlet {
		return inlet_new(self, dest, from, to) orelse Err.ObjectNewInlet;
	}
	extern fn inlet_new(*Self, *Pd, ?*Symbol, ?*Symbol) ?*Inlet;

	pub fn inletPointer(self: *Self, gp: *GPointer) Err!*Inlet {
		return pointerinlet_new(self, gp) orelse Err.ObjectNewInletPointer;
	}
	extern fn pointerinlet_new(*Self, *GPointer) ?*Inlet;

	pub fn inletFloat(self: *Self, fp: *Float) Err!*Inlet {
		return floatinlet_new(self, fp) orelse Err.ObjectNewInletFloat;
	}
	extern fn floatinlet_new(*Self, *Float) ?*Inlet;

	pub fn inletSymbol(self: *Self, sym: **Symbol) Err!*Inlet {
		return symbolinlet_new(self, sym) orelse Err.ObjectNewInletSymbol;
	}
	extern fn symbolinlet_new(*Self, **Symbol) ?*Inlet;

	pub fn inletSignal(self: *Self, f: Float) Err!*Inlet {
		return signalinlet_new(self, f) orelse Err.ObjectNewInletSignal;
	}
	extern fn signalinlet_new(*Self, Float) ?*Inlet;

	pub fn xPix(self: *Self, glist: *cnv.GList) i32 {
		return text_xpix(self, glist);
	}
	extern fn text_xpix(*Self, *cnv.GList) c_int;

	pub fn yPix(self: *Self, glist: *cnv.GList) i32 {
		return text_ypix(self, glist);
	}
	extern fn text_ypix(*Self, *cnv.GList) c_int;

	pub fn inletFloatArg(
		self: *Self,
		fp: *Float,
		av: []const Atom,
		which: usize
	) Err!*Inlet {
		fp.* = floatArg(av, which);
		return self.inletFloat(fp);
	}

	pub fn inletSymbolArg(
		self: *Self,
		sp: **Symbol,
		av: []const Atom,
		which: usize
	) Err!*Inlet {
		sp.* = symbolArg(av, which);
		return self.inletSymbol(sp);
	}
};


// ---------------------------------- Outlet -----------------------------------
// -----------------------------------------------------------------------------
pub const Outlet = opaque {
	const Self = @This();

	pub const bang = outlet_bang;
	extern fn outlet_bang(*Self) void;

	pub const pointer = outlet_pointer;
	extern fn outlet_pointer(*Self, *GPointer) void;

	pub const float = outlet_float;
	extern fn outlet_float(*Self, Float) void;

	pub const symbol = outlet_symbol;
	extern fn outlet_symbol(*Self, *Symbol) void;

	pub fn list(self: *Self, sym: ?*Symbol, av: []Atom) void {
		outlet_list(self, sym, @intCast(av.len), av.ptr);
	}
	extern fn outlet_list(*Self, ?*Symbol, c_uint, [*]Atom) void;

	pub fn anything(self: *Self, sym: *Symbol, av: []Atom) void {
		outlet_anything(self, sym, @intCast(av.len), av.ptr);
	}
	extern fn outlet_anything(*Self, *Symbol, c_uint, [*]Atom) void;

	/// Get the outlet's declared symbol
	pub const getSymbol = outlet_getsymbol;
	extern fn outlet_getsymbol(*Self) *Symbol;

	pub const free = outlet_free;
	extern fn outlet_free(*Self) void;
};


// ------------------------------------ Pd -------------------------------------
// -----------------------------------------------------------------------------
pub const Pd = extern struct {
	const Self = @This();

	_: *const imp.Class,

	pub const free = pd_free;
	extern fn pd_free(*Self) void;

	pub const bind = pd_bind;
	extern fn pd_bind(*Self, *Symbol) void;

	pub const unbind = pd_unbind;
	extern fn pd_unbind(*Self, *Symbol) void;

	pub const pushSymbol = pd_pushsym;
	extern fn pd_pushsym(*Self) void;

	pub const popSymbol = pd_popsym;
	extern fn pd_popsym(*Self) void;

	pub const bang = pd_bang;
	extern fn pd_bang(*Self) void;

	pub const pointer = pd_pointer;
	extern fn pd_pointer(*Self, *GPointer) void;

	pub const float = pd_float;
	extern fn pd_float(*Self, Float) void;

	pub const symbol = pd_symbol;
	extern fn pd_symbol(*Self, *Symbol) void;

	pub fn list(self: *Self, sym: ?*Symbol, av: []Atom) void {
		pd_list(self, sym, @intCast(av.len), av.ptr);
	}
	extern fn pd_list(*Self, ?*Symbol, c_uint, [*]Atom) void;

	pub fn anything(self: *Self, sym: *Symbol, av: []Atom) void {
		pd_anything(self, sym, @intCast(av.len), av.ptr);
	}
	extern fn pd_anything(*Self, *Symbol, c_uint, [*]Atom) void;

	pub fn typedMess(self: *Self, sym: ?*Symbol, av: []Atom) void {
		pd_typedmess(self, sym, @intCast(av.len), av.ptr);
	}
	extern fn pd_typedmess(*Self, ?*Symbol, c_uint, [*]Atom) void;

	pub fn forwardMess(self: *Self, av: []Atom) void {
		pd_forwardmess(self, @intCast(av.len), av.ptr);
	}
	extern fn pd_forwardmess(*Self, c_uint, [*]Atom) void;

	/// Checks that a pd is indeed a patchable object, and returns
	/// it, correctly typed, or null if the check failed.
	pub const checkObject = pd_checkobject;
	extern fn pd_checkobject(*Self) ?*Object;

	pub const parentWidget = pd_getparentwidget;
	extern fn pd_getparentwidget(*Self) ?*const cnv.parent.WidgetBehavior;

	pub fn vStub(
		self: *Self,
		dest: [:0]const u8,
		key: *anyopaque,
		fmt: [:0]const u8,
		args: anytype
	) void {
		@call(.auto, pdgui_stub_vnew, .{ self, dest.ptr, key, fmt.ptr } ++ args);
	}
	extern fn pdgui_stub_vnew(*Self, [*:0]const u8, *anyopaque, [*:0]const u8, ...) void;

	pub const func = getfn;
	extern fn getfn(*const Self, *Symbol) GotFn;

	pub const zFunc = zgetfn;
	extern fn zgetfn(*const Self, *Symbol) GotFn;

	/// This is externally available, but note that it might later disappear; the
	/// whole "newest" thing is a hack which needs to be redesigned.
	pub const newest = pd_newest; // static
	extern fn pd_newest() *Pd;

	pub const new = imp.Class.pd;
};


// --------------------------------- Resample ----------------------------------
// -----------------------------------------------------------------------------
pub const Resample = extern struct {
	const Self = @This();

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

	pub const init = resample_init;
	extern fn resample_init(*Self) void;

	pub const free = resample_free;
	extern fn resample_free(*Self) void;

	pub fn dsp(self: *Self, in: []Sample, out: []Sample, conv: Converter) void {
		resample_dsp(self, in.ptr, @intCast(in.len), out.ptr, @intCast(out.len), conv);
	}
	extern fn resample_dsp(*Self, *Sample, c_uint, *Sample, c_uint, Converter) void;

	pub fn dspFrom(self: *Self, in: []Sample, out_len: usize, conv: Converter) void {
		resamplefrom_dsp(self, in.ptr, @intCast(in.len), @intCast(out_len), conv);
	}
	extern fn resamplefrom_dsp(*Self, *Sample, c_uint, c_uint, Converter) void;

	pub fn dspTo(self: *Self, out: []Sample, in_len: usize, conv: Converter) void {
		resampleto_dsp(self, out.ptr, @intCast(in_len), @intCast(out.len), conv);
	}
	extern fn resampleto_dsp(*Self, *Sample, c_uint, c_uint, Converter) void;
};


// ---------------------------------- Signal -----------------------------------
// -----------------------------------------------------------------------------
pub const Signal = extern struct {
	const Self = @This();

	pub const Error = error {
		SignalNew,
	};
	const Err = Self.Error;

	len: c_uint,
	vec: [*]Sample,
	srate: Float,
	nchans: c_uint,
	overlap: c_int,
	refcount: c_uint,
	isborrowed: c_uint,
	isscalar: c_uint,
	borrowedfrom: ?*Self,
	nextfree: ?*Self,
	nextused: ?*Self,
	nalloc: c_uint,

	/// Pop an audio signal from free list or create a new one.
	///
	/// If `scalarptr` is nonzero, it's a pointer to a scalar owned by the
	/// tilde object. In this case, we neither allocate nor free it.
	/// Otherwise, if `length` is zero, return a "borrowed"
	/// signal whose buffer and size will be obtained later via
	/// `signal_setborrowed()`.
	pub fn new(
		length: u32,
		nchans: u32,
		samplerate: Float,
		scalarptr: *Sample
	) Err!*Self {
		return signal_new(length, nchans, samplerate, scalarptr) orelse Err.SignalNew;
	}
	extern fn signal_new(c_uint, c_uint, Float, *Sample) ?*Self;

	/// Only use this in the context of dsp routines to set number of channels
	/// on output signal - we assume it's currently a pointer to the null signal.
	pub fn setMultiOut(sig: **Self, nchans: u32) void {
		signal_setmultiout(sig, nchans);
	}
	extern fn signal_setmultiout(**Self, c_uint) void;
};

pub const signal = Signal.new;


// ---------------------------------- Symbol -----------------------------------
// -----------------------------------------------------------------------------
pub const Symbol = extern struct {
	const Self = @This();

	name: [*:0]const u8,
	thing: ?*Pd,
	next: ?*Self,

	pub const gen = gensym;
	extern fn gensym([*:0]const u8) *Self; // could run out of memory
};

pub const symbol = Symbol.gen;

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
pub const GuiCallbackFn = ?*const fn (*GObj, *cnv.GList) callconv(.C) void;

pub fn blockSize() u32 {
	return sys_getblksize();
}
extern fn sys_getblksize() c_uint;

pub const sampleRate = sys_getsr;
extern fn sys_getsr() Float;

pub fn inChannels() u32 {
	return sys_get_inchannels();
}
extern fn sys_get_inchannels() c_uint;

pub fn outChannels() u32 {
	return sys_get_outchannels();
}
extern fn sys_get_outchannels() c_uint;

/// If some GUI object is having to do heavy computations, it can tell
/// us to back off from doing more updates by faking a big one itself.
pub fn pretendGuiBytes(n: u32) void {
	sys_pretendguibytes(n);
}
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
extern fn sys_trylock() Result;

pub const hostFontSize = sys_hostfontsize;
extern fn sys_hostfontsize(fontsize: c_uint, zoom: c_uint) c_uint;

pub const zoomFontWidth = sys_zoomfontwidth;
extern fn sys_zoomfontwidth(fontsize: c_uint, zoom: c_uint, worstcase: c_uint) c_uint;

pub const zoomFontHeight = sys_zoomfontheight;
extern fn sys_zoomfontheight(fontsize: c_uint, zoom: c_uint, worstcase: c_uint) c_uint;

pub const fontWidth = sys_fontwidth;
extern fn sys_fontwidth(fontsize: c_uint) c_uint;

pub const fontHeight = sys_fontheight;
extern fn sys_fontheight(fontsize: c_uint) c_uint;

pub fn isAbsolutePath(dir: [:0]const u8) bool {
	return (sys_isabsolutepath(dir.ptr) != 0);
}
extern fn sys_isabsolutepath([*:0]const u8) Bool;

pub const currentDir = canvas_getcurrentdir;
extern fn canvas_getcurrentdir() *Symbol;

/// DSP can be suspended before, and resumed after, operations which
/// might affect the DSP chain.  For example, we suspend before loading and
/// resume afterward, so that DSP doesn't get resorted for every DSP object
/// in the patch.
pub fn suspendDsp() bool {
	return (canvas_suspend_dsp() != 0);
}
extern fn canvas_suspend_dsp() Bool;

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
extern fn pd_getdspstate() Bool;


// ----------------------------------- Value -----------------------------------
// -----------------------------------------------------------------------------
pub const value = struct {
	pub const Error = error {
		ValueGet,
		ValueSet,
	};
	const Err = @This().Error;

	/// Get a pointer to a named floating-point variable.  The variable
	/// belongs to a `vcommon` object, which is created if necessary.
	pub const from = value_get;
	extern fn value_get(*Symbol) *Float;

	pub const release = value_release;
	extern fn value_release(*Symbol) void;

	/// obtain the float value of a "value" object
	pub fn get(sym: *Symbol, f: *Float) Err!void {
		if (value_getfloat(sym, f) != 0)
			return Err.ValueGet;
	}
	extern fn value_getfloat(*Symbol, *Float) Result;

	pub fn set(sym: *Symbol, f: Float) Err!void {
		if (value_setfloat(sym, f) != 0)
			return Err.ValueSet;
	}
	extern fn value_setfloat(*Symbol, Float) Result;
};


// ----------------------------------- Misc ------------------------------------
// -----------------------------------------------------------------------------
pub const object_maker = &pd_objectmaker;
pub extern var pd_objectmaker: Pd;

pub const canvas_maker = &pd_canvasmaker;
pub extern var pd_canvasmaker: Pd;

pub const GotFn = ?*const fn (*anyopaque, ...) callconv(.C) void;
pub const GotFn1 = ?*const fn (*anyopaque, *anyopaque) callconv(.C) void;
pub const GotFn2 = ?*const fn (*anyopaque, *anyopaque, *anyopaque) callconv(.C) void;
pub const GotFn3 = ?*const fn (*anyopaque, *anyopaque, *anyopaque, *anyopaque) callconv(.C) void;
pub const GotFn4 = ?*const fn (*anyopaque, *anyopaque, *anyopaque, *anyopaque, *anyopaque) callconv(.C) void;
pub const GotFn5 = ?*const fn (*anyopaque, *anyopaque, *anyopaque, *anyopaque, *anyopaque, *anyopaque) callconv(.C) void;

pub const nullFn = nullfn;
extern fn nullfn() void;

pub const font: [*:0]u8 = @extern([*:0]u8, .{ .name = "sys_font" });
pub const font_weight: [*:0]u8 = @extern([*:0]u8, .{ .name = "sys_fontweight" });

pub const post = struct {
	pub fn do(fmt: [:0]const u8, args: anytype) void {
		@call(.auto, post_, .{ fmt.ptr } ++ args);
	}
	const post_ = @extern(
		*const fn([*:0]const u8, ...) callconv(.C) void, .{ .name = "post" });

	pub fn start(fmt: [:0]const u8, args: anytype) void {
		@call(.auto, startpost, .{ fmt.ptr } ++ args);
	}
	extern fn startpost([*:0]const u8, ...) void;

	pub fn string(str: [:0]const u8) void {
		poststring(str.ptr);
	}
	extern fn poststring([*:0]const u8) void;

	pub const float = postfloat;
	extern fn postfloat(f: Float) void;

	pub fn atom(av: []const Atom) void {
		postatom(av.len, av.ptr);
	}
	extern fn postatom(c_uint, [*]const Atom) void;

	pub const end = endpost;
	extern fn endpost() void;

	pub fn bug(fmt: [:0]const u8, args: anytype) void {
		@call(.auto, bug_, .{ fmt.ptr } ++ args);
	}
	const bug_ = @extern(
		*const fn([*:0]const u8, ...) callconv(.C) void, .{ .name = "bug" });

	pub fn err(self: ?*const anyopaque, fmt: [:0]const u8, args: anytype) void {
		@call(.auto, pd_error, .{ self, fmt.ptr } ++ args);
	}
	extern fn pd_error(?*const anyopaque, fmt: [*:0]const u8, ...) void;

	pub const LogLevel = enum(c_uint) {
		critical,
		err,
		normal,
		debug,
		verbose,
	};

	pub fn log(
		obj: ?*const anyopaque,
		lvl: LogLevel,
		fmt: [:0]const u8,
		args: anytype
	) void {
		@call(.auto, logpost, .{ obj, lvl, fmt.ptr } ++ args);
	}
	extern fn logpost(?*const anyopaque, LogLevel, [*:0]const u8, ...) void;
};

/// Get a number unique to the (clock, MIDI, GUI, etc.) event we're on
pub fn eventNumber() u32 {
	return sched_geteventno();
}
extern fn sched_geteventno() c_uint;

/// sys_idlehook is a hook the user can fill in to grab idle time.  Return
/// nonzero if you actually used the time; otherwise we're really really idle and
/// will now sleep.
pub extern var sys_idlehook: ?*const fn () callconv(.C) c_int;

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

	pub fn fft(n: usize, real: [*]Sample, imag: [*]Sample) void {
		mayer_fft(@intCast(n), real, imag);
	}
	extern fn mayer_fft(c_uint, [*]Sample, [*]Sample) void;

	pub fn ifft(n: usize, real: [*]Sample, imag: [*]Sample) void {
		mayer_ifft(@intCast(n), real, imag);
	}
	extern fn mayer_ifft(n: c_uint, real: [*]Sample, imag: [*]Sample) void;

	pub fn realfft(real: []Sample) void {
		mayer_realfft(@intCast(real.len), real.ptr);
	}
	extern fn mayer_realfft(n: c_uint, real: [*]Sample) void;

	pub fn realifft(real: []Sample) void {
		mayer_realifft(real.len, real.ptr);
	}
	extern fn mayer_realifft(n: c_uint, real: [*]Sample) void;
};

pub fn fft(buf: []Float, inverse: bool) void {
	pd_fft(buf.ptr, @intCast(buf.len), @intFromBool(inverse));
}
extern fn pd_fft(buf: *Float, n_points: c_uint, inverse: c_uint) void;

const ushift = std.meta.Int(.unsigned, @log2(@as(f32, @bitSizeOf(usize))));
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

pub const vMess = pdgui_vmess;
extern fn pdgui_vmess(destination: ?[*:0]const u8, fmt: [*:0]const u8, ...) void;

pub const deleteStubForKey = pdgui_stub_deleteforkey;
extern fn pdgui_stub_deleteforkey(key: *anyopaque) void;

pub const cExtern = c_extern;
extern fn c_extern(
	cls: *imp.Class,
	newroutine: NewMethod,
	freeroutine: Method,
	name: *Symbol,
	size: usize,
	tiny: c_int,
	arg1: Atom.Type,
	...
) void;

pub const cAddMess = c_addmess;
extern fn c_addmess(func: Method, sel: *Symbol, arg1: Atom.Type, ...) void;

const flt_bits = @bitSizeOf(Float);
const exp_bits = std.math.floatExponentBits(Float);
const sign_bit = 1;
const exp_mask = ((1 << exp_bits) - 1) << (flt_bits - exp_bits - sign_bit);
const bos_mask = 1 << (flt_bits - 3);

pub const BigOrSmall = extern union {
	f: Float,
	ui: std.meta.Int(.unsigned, flt_bits),
};

pub fn badFloat(f: Float) bool {
	var pun = BigOrSmall{ .f = f };
	pun.ui &= exp_mask;
	return (f != 0 and (pun.ui == 0 or pun.ui == exp_mask));
}

pub fn bigOrSmall(f: Float) bool {
	const pun = BigOrSmall{ .f = f };
	return ((pun.ui & bos_mask) == ((pun.ui >> 1) & bos_mask));
}

test "bad float" {
	try std.testing.expect(badFloat((BigOrSmall{ .ui = exp_mask }).f)); // infinity
	try std.testing.expect(badFloat((BigOrSmall{ .ui = exp_mask + 1 }).f)); // NaN
	try std.testing.expect(badFloat((BigOrSmall{ .ui = 1 }).f)); // denormal
	try std.testing.expect(!badFloat(123.45)); // good float
}

test "big or small" {
	const big = if (@bitSizeOf(Float) == 64) 0x1p513 else 0x1p65;
	const small = if (@bitSizeOf(Float) == 64) 0x1p-512 else 0x1p-64;
	try std.testing.expect(bigOrSmall(big));
	try std.testing.expect(bigOrSmall(small));
	try std.testing.expect(!bigOrSmall(123.45));
}

pub const Template = opaque {};
pub const Instance = extern struct {
	pub const Midi = opaque {};
	pub const Inter = opaque {};
	pub const Ugen = opaque {};
	pub const Canvas = opaque {};
	pub const Stuff = opaque {};

	systime: f64,
	clock_setlist: *Clock,
	canvaslist: *cnv.GList,
	templatelist: *Template,
	instanceno: c_uint,
	symhash: **Symbol,
	midi: ?*Midi,
	inter: ?*Inter,
	ugen: ?*Ugen,
	gui: ?*Canvas,
	stuff: ?*Stuff,
	newest: *Pd,
	islocked: c_uint,

	pub const main = &pd_maininstance;
};
pub extern const pd_maininstance: Instance;

pub const max_string = 1000;
pub const max_arg = 5;
pub const max_logsig = 32;
pub const max_sigsize = 1 << max_logsig;
pub const threads = 1;
