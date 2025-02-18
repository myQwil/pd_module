const pd = @import("pd.zig");
const cnv = @import("canvas.zig");

const strlen = @import("std").mem.len;

pub const Error = Class.Error;

const Pd = pd.Pd;
const GObj = pd.GObj;
const GPointer = pd.GPointer;
const BinBuf = pd.BinBuf;

const NewMethod = pd.NewMethod;
const Method = pd.Method;
const GotFn = pd.GotFn;

const Atom = pd.Atom;
const Float = pd.Float;
const Symbol = pd.Symbol;


// ----------------------------------- Class -----------------------------------
// -----------------------------------------------------------------------------
pub const MethodEntry = extern struct {
	name: *Symbol,
	fun: GotFn,
	arg: [pd.max_arg:0]u8,
};

pub const Class = extern struct {
	const Self = @This();

	pub const Error = error {
		ClassPd,
		ClassNew,
	};
	const Err = Self.Error;

	pub const fnBang = ?*const fn (*Pd) callconv(.C) void;
	pub const fnPointer = ?*const fn (*Pd, *GPointer) callconv(.C) void;
	pub const fnFloat = ?*const fn (*Pd, Float) callconv(.C) void;
	pub const fnSymbol = ?*const fn (*Pd, *Symbol) callconv(.C) void;
	pub const fnList = ?*const fn (*Pd, ?*Symbol, c_uint, [*]Atom) callconv(.C) void;
	pub const fnAny = ?*const fn (*Pd, *Symbol, c_uint, [*]Atom) callconv(.C) void;
	pub const fnFree = ?*const fn (*Self) callconv(.C) void;
	pub const fnSave = ?*const fn (*GObj, *BinBuf) callconv(.C) void;
	pub const fnProperties = ?*const fn (*GObj, *cnv.GList) callconv(.C) void;

	name: *Symbol,
	helpname: *Symbol,
	externdir: *Symbol,
	size: usize,
	methods: [*]MethodEntry,
	nmethod: c_uint,
	method_free: Method,
	method_bang: fnBang,
	method_pointer: fnPointer,
	method_float: fnFloat,
	method_symbol: fnSymbol,
	method_list: fnList,
	method_any: fnAny,
	wb: ?*const cnv.WidgetBehavior,
	pwb: ?*const cnv.parent.WidgetBehavior,
	fn_save: fnSave,
	fn_properties: fnProperties,
	next: ?*Class,
	float_signal_in: c_uint,
	flags: packed struct(u8) {
		/// true if is a gobj
		gobj: bool,
		/// true if we have a t_object header
		patchable: bool,
		/// if so, true if drawing first inlet
		first_in: bool,
		/// drawing command for a template
		draw_command: bool,
		/// can deal with multichannel sigs
		multichannel: bool,
		/// don't promote scalars to signals
		no_promote_sig: bool,
		/// don't promote the main (left) inlet to signals
		no_promote_left: bool,
		unused: u1,
	},
	fn_free: fnFree,

	pub const Options = struct {
		/// non-canvasable pd such as an inlet
		bare: bool = false,
		/// pd that can belong to a canvas
		gobj: bool = false,
		/// pd that also can have inlets and outlets
		patchable: bool = false,

		/// suppress left inlet
		no_inlet: bool = false,
		/// can deal with multichannel signals
		multichannel: bool = false,
		/// don't promote scalars to signals
		no_promote_sig: bool = false,
		/// don't promote the main (left) inlet to signals
		no_promote_left: bool = false,

		fn toInt(self: Options) c_uint {
			return @intFromBool(self.bare)
				| (@as(u2, @intFromBool(self.gobj)) << 1)
				| (@as(u2, @intFromBool(self.patchable)) * 3)
				| (@as(u4, @intFromBool(self.no_inlet)) << 3)
				| (@as(u5, @intFromBool(self.multichannel)) << 4)
				| (@as(u6, @intFromBool(self.no_promote_sig)) << 5)
				| (@as(u7, @intFromBool(self.no_promote_left)) << 6);
		}
	};

	pub const free = class_free;
	extern fn class_free(c: *Self) void;

	pub const addBang = class_addbang;
	extern fn class_addbang(*Self, Method) void;

	pub const addPointer = class_addpointer;
	extern fn class_addpointer(*Self, Method) void;

	pub const addFloat = class_doaddfloat;
	extern fn class_doaddfloat(*Self, Method) void;

	pub const addSymbol = class_addsymbol;
	extern fn class_addsymbol(*Self, Method) void;

	pub const addList = class_addlist;
	extern fn class_addlist(*Self, Method) void;

	pub const addAnything = class_addanything;
	extern fn class_addanything(*Self, Method) void;

	pub const setHelpSymbol = class_sethelpsymbol;
	extern fn class_sethelpsymbol(*Self, *Symbol) void;

	pub const setWidget = class_setwidget;
	extern fn class_setwidget(*Self, *const cnv.WidgetBehavior) void;

	pub const setParentWidget = class_setparentwidget;
	extern fn class_setparentwidget(*Self, *const cnv.parent.WidgetBehavior) void;

	pub fn getName(self: *const Self) [:0]const u8 {
		const str = class_getname(self);
		return str[0..strlen(str) :0];
	}
	extern fn class_getname(*const Self) [*:0]const u8;

	pub fn helpName(self: *const Self) [:0]const u8 {
		const str = class_gethelpname(self);
		return str[0..strlen(str) :0];
	}
	extern fn class_gethelpname(*const Self) [*:0]const u8;

	pub fn helpDir(self: *const Self) [:0]const u8 {
		const str = class_gethelpdir(self);
		return str[0..strlen(str) :0];
	}
	extern fn class_gethelpdir(*const Self) [*:0]const u8;

	pub const setDrawCommand = class_setdrawcommand;
	extern fn class_setdrawcommand(*Self) void;

	pub fn doMainSignalIn(self: *Self, onset: u32) void {
		class_domainsignalin(self, onset);
	}
	extern fn class_domainsignalin(*Self, c_uint) void;

	pub const setSaveFn = class_setsavefn;
	extern fn class_setsavefn(*Self, fnSave) void;

	pub const saveFn = class_getsavefn;
	extern fn class_getsavefn(*const Self) fnSave;

	pub const setPropertiesFn = class_setpropertiesfn;
	extern fn class_setpropertiesfn(*Self, fnProperties) void;

	pub const propertiesFn = class_getpropertiesfn;
	extern fn class_getpropertiesfn(*const Self) fnProperties;

	pub const setFreeFn = class_setfreefn;
	extern fn class_setfreefn(*Self, fnFree) void;

	pub fn pd(self: *Self) Err!*Pd {
		return pd_new(self) orelse Err.ClassPd;
	}
	extern fn pd_new(*Self) ?*Pd;

	pub fn isDrawCommand(self: *const Self) bool {
		return (class_isdrawcommand(self) != 0);
	}
	extern fn class_isdrawcommand(*const Self) c_uint;

	pub fn find(self: *const Self, sym: *Symbol) ?*Pd {
		return pd_findbyclass(sym, self);
	}
	extern fn pd_findbyclass(*Symbol, *const Self) ?*Pd;

	pub fn addMethod(
		self: *Self,
		meth: Method,
		sym: *Symbol,
		comptime args: []const Atom.Type,
	) void {
		@call(.auto, class_addmethod, .{ self, meth, sym } ++ Atom.Type.tuple(args));
	}
	extern fn class_addmethod(*Self, Method, *Symbol, c_uint, ...) void;

	pub fn new(
		sym: *Symbol,
		newm: NewMethod,
		freem: Method,
		size: usize,
		opt: Class.Options,
		comptime args: []const Atom.Type,
	) Err!*Self {
		return @call(.auto, classNew,
			.{ sym, newm, freem, size, opt.toInt() } ++ Atom.Type.tuple(args))
			orelse Err.ClassNew;
	}
	extern fn class_new(*Symbol, NewMethod, Method, usize, c_uint, c_uint, ...) ?*Self;
	extern fn class_new64(*Symbol, NewMethod, Method, usize, c_uint, c_uint, ...) ?*Self;
	const classNew = if (@bitSizeOf(Float) == 64) class_new64 else class_new;

	pub const garray: **Self = @extern(**Self, .{ .name = "garray_class" });
	pub const scalar: **Self = @extern(**Self, .{ .name = "scalar_class" });
	pub const global: **Self = @extern(**Self, .{ .name = "glob_pdobject" });
};
