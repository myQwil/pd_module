const std = @import("std");
const m = @import("pd.zig");
const cnv = @import("canvas.zig");

const strlen = @import("std").mem.len;

const Pd = m.Pd;
const GObj = m.GObj;
const GPointer = m.GPointer;
const BinBuf = m.BinBuf;

const NewMethod = m.NewMethod;
const Method = m.Method;
const GotFn = m.GotFn;

const Atom = m.Atom;
const Float = m.Float;
const Symbol = m.Symbol;

pub fn printStruct(T: type, sym: *Symbol) void {
	const info = @typeInfo(T).@"struct";
	const Field = struct{ name: []const u8, offset: usize, type: type };
	const fields: [info.fields.len]Field = comptime blk: {
		var fields: [info.fields.len]Field = undefined;
		for (info.fields, 0..) |field, i| {
			fields[i] = .{
				.name = field.name,
				.offset = @offsetOf(T, field.name),
				.type = field.type,
			};
		}
		for (0..fields.len) |i| {
			for (i + 1..fields.len) |j| {
				if (fields[j].offset < fields[i].offset) {
					const temp = fields[i];
					fields[i] = fields[j];
					fields[j] = temp;
				}
			}
		}
		break :blk fields;
	};
	m.post.do("[{s}]: {s}", .{ sym.name, @typeName(T) });
	inline for (fields) |field| {
		m.post.do("    {s}: {s} -> {}",
			.{ field.name, @typeName(field.type), field.offset });
	}
}


// ----------------------------------- Class -----------------------------------
// -----------------------------------------------------------------------------
pub const MethodEntry = extern struct {
	name: *Symbol,
	fun: GotFn,
	arg: [m.max_arg:0]u8,
};

pub const Class = extern struct {
	pub const Error = error {
		ClassPd,
		ClassNew,
	};

	pub const fnBang = ?*const fn (*Pd) callconv(.c) void;
	pub const fnPointer = ?*const fn (*Pd, *GPointer) callconv(.c) void;
	pub const fnFloat = ?*const fn (*Pd, Float) callconv(.c) void;
	pub const fnSymbol = ?*const fn (*Pd, *Symbol) callconv(.c) void;
	pub const fnList = ?*const fn (*Pd, ?*Symbol, c_uint, [*]Atom) callconv(.c) void;
	pub const fnAny = ?*const fn (*Pd, *Symbol, c_uint, [*]Atom) callconv(.c) void;
	pub const fnFree = ?*const fn (*Class) callconv(.c) void;
	pub const fnSave = ?*const fn (*GObj, *BinBuf) callconv(.c) void;
	pub const fnProperties = ?*const fn (*GObj, *cnv.GList) callconv(.c) void;

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
		_unused: u1,
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
	extern fn class_free(c: *Class) void;

	pub const addBang = class_addbang;
	extern fn class_addbang(*Class, Method) void;

	pub const addPointer = class_addpointer;
	extern fn class_addpointer(*Class, Method) void;

	pub const addFloat = class_doaddfloat;
	extern fn class_doaddfloat(*Class, Method) void;

	pub const addSymbol = class_addsymbol;
	extern fn class_addsymbol(*Class, Method) void;

	pub const addList = class_addlist;
	extern fn class_addlist(*Class, Method) void;

	pub const addAnything = class_addanything;
	extern fn class_addanything(*Class, Method) void;

	pub const setHelpSymbol = class_sethelpsymbol;
	extern fn class_sethelpsymbol(*Class, *Symbol) void;

	pub const setWidget = class_setwidget;
	extern fn class_setwidget(*Class, *const cnv.WidgetBehavior) void;

	pub const setParentWidget = class_setparentwidget;
	extern fn class_setparentwidget(*Class, *const cnv.parent.WidgetBehavior) void;

	pub const getName = class_getname;
	extern fn class_getname(*const Class) [*:0]const u8;

	pub const getHelpName = class_gethelpname;
	extern fn class_gethelpname(*const Class) [*:0]const u8;

	pub const getHelpDir = class_gethelpdir;
	extern fn class_gethelpdir(*const Class) [*:0]const u8;

	pub const setDrawCommand = class_setdrawcommand;
	extern fn class_setdrawcommand(*Class) void;

	pub const doMainSignalIn = class_domainsignalin;
	extern fn class_domainsignalin(*Class, c_uint) void;

	pub const setSaveFn = class_setsavefn;
	extern fn class_setsavefn(*Class, fnSave) void;

	pub const saveFn = class_getsavefn;
	extern fn class_getsavefn(*const Class) fnSave;

	pub const setPropertiesFn = class_setpropertiesfn;
	extern fn class_setpropertiesfn(*Class, fnProperties) void;

	pub const propertiesFn = class_getpropertiesfn;
	extern fn class_getpropertiesfn(*const Class) fnProperties;

	pub const setFreeFn = class_setfreefn;
	extern fn class_setfreefn(*Class, fnFree) void;

	pub fn pd(self: *Class) Error!*Pd {
		return pd_new(self) orelse Error.ClassPd;
	}
	extern fn pd_new(*Class) ?*Pd;

	pub fn isDrawCommand(self: *const Class) bool {
		return (class_isdrawcommand(self) != 0);
	}
	extern fn class_isdrawcommand(*const Class) c_uint;

	pub fn find(self: *const Class, sym: *Symbol) ?*Pd {
		return pd_findbyclass(sym, self);
	}
	extern fn pd_findbyclass(*Symbol, *const Class) ?*Pd;

	pub fn addMethod(
		self: *Class,
		meth: Method,
		sym: *Symbol,
		comptime args: []const Atom.Type,
	) void {
		@call(.auto, class_addmethod, .{ self, meth, sym } ++ Atom.Type.tuple(args));
	}
	extern fn class_addmethod(*Class, Method, *Symbol, c_uint, ...) void;

	pub fn new(
		T: type,
		name: [:0]const u8,
		comptime args: []const Atom.Type,
		new_method: ?*const m.NewFn(T, args),
		free_method: ?*const fn(*T) callconv(.c) void,
		options: Class.Options,
	) Error!*Class {
		// printStruct(T, sym); // uncomment this to view struct field order
		const sym: *Symbol = .gen(name.ptr);
		const newm: NewMethod = @ptrCast(new_method);
		const freem: Method = @ptrCast(free_method);
		return @call(.auto, classNew,
			.{ sym, newm, freem, @sizeOf(T), options.toInt() } ++ Atom.Type.tuple(args)
		) orelse Error.ClassNew;
	}
	extern fn class_new(*Symbol, NewMethod, Method, usize, c_uint, c_uint, ...) ?*Class;
	extern fn class_new64(*Symbol, NewMethod, Method, usize, c_uint, c_uint, ...) ?*Class;
	const classNew = if (@bitSizeOf(Float) == 64) class_new64 else class_new;
};
