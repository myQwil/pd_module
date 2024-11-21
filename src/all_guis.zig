const pd = @import("pd.zig");
const cnv = @import("canvas.zig");

pub const Error = Gui.Error;

const Class = @import("imp.zig").Class;
const Atom = pd.Atom;
const Float = pd.Float;
const Symbol = pd.Symbol;
const GList = cnv.GList;
const GObj = pd.GObj;

pub const min_size: u32 = 8;
pub const max_size: u32 = 1000;
pub const max_num_len: u32 = 32;
pub const io_height: u32 = cnv.i_height;

pub fn isFloat(av: []Atom) bool {
	for (av) |*a| {
		if (a.type != .float) {
			return false;
		}
	}
	return true;
}

pub fn isSymbolOrFloat(av: []Atom) bool {
	for (av) |*a| {
		if (a.type != .float and a.type != .symbol) {
			return false;
		}
	}
	return true;
}


// ------------------------------------ Gui ------------------------------------
// -----------------------------------------------------------------------------
const DrawMode = enum(c_uint) {
	update,
	move,
	new,
	select,
	erase,
	config,
	io,
};

pub const FunPtr = ?*const fn (*anyopaque, *GList, DrawMode) callconv(.C) void;
pub const DrawFunPtr = ?*const fn (*anyopaque, *GList) callconv(.C) void;
const Private = opaque {};

pub const DrawFunctions = extern struct {
	new: DrawFunPtr = null,
	config: DrawFunPtr = null,
	iolets: FunPtr = null,
	update: DrawFunPtr = null,
	select: DrawFunPtr = null,
	erase: DrawFunPtr = null,
	move: DrawFunPtr = null,
};

pub const FontStyleFlags = packed struct(u32) {
	font_style: u6,
	rcv_able: bool,
	snd_able: bool,
	lab_is_unique: bool,
	rcv_is_unique: bool,
	snd_is_unique: bool,
	lab_arg_tail_len: u6,
	lab_is_arg_num: u6,
	shiftdown: bool,
	selected: bool,
	finemoved: bool,
	put_in2out: bool,
	change: bool,
	thick: bool,
	lin0_log1: bool,
	steady: bool,
	_padding: u1,
};

pub const InitSymArgs = packed struct(u32) {
	loadinit: bool,
	rcv_arg_tail_len: u6,
	snd_arg_tail_len: u6,
	rcv_is_arg_num: u6,
	snd_is_arg_num: u6,
	scale: bool,
	flashed: bool,
	locked: bool,
	_padding: u4,
};

pub const Gui = extern struct {
	const Self = @This();

	pub const Error = error {
		GuiNew,
	};
	const Err = Self.Error;

	obj: pd.Object,
	glist: *GList,
	draw: FunPtr,
	h: c_uint,
	w: c_uint,
	private: *Private,
	ldx: c_int,
	ldy: c_int,
	font: [pd.max_string-1:0]u8,
	fsf: FontStyleFlags,
	fontsize: c_uint,
	isa: InitSymArgs,
	fcol: c_uint,
	bcol: c_uint,
	lcol: c_uint,
	snd: ?*Symbol,
	rcv: ?*Symbol,
	lab: *Symbol,
	snd_unexpanded: *Symbol,
	rcv_unexpanded: *Symbol,
	lab_unexpanded: *Symbol,
	binbufindex: c_uint,
	labelbindex: c_uint,

	pub const free = iemgui_free;
	extern fn iemgui_free(*Self) void;

	pub const verifySendNotReceive = iemgui_verify_snd_ne_rcv;
	extern fn iemgui_verify_snd_ne_rcv(*Self) void;

	/// Get the unexpanded versions of the symbols. Initialize them if necessary.
	pub const symToDollarArg = iemgui_all_sym2dollararg;
	extern fn iemgui_all_sym2dollararg(*Self, [*]*Symbol) void;

	pub const dollarArgToSym = iemgui_all_dollararg2sym;
	extern fn iemgui_all_dollararg2sym(*Self, [*]*Symbol) void;

	pub const getName = iemgui_new_dogetname;
	extern fn iemgui_new_dogetname(*Self, indx: c_uint, argv: [*]Atom) ?*Symbol;

	pub const getNames = iemgui_new_getnames;
	extern fn iemgui_new_getnames(*Self, indx: c_uint, argv: ?[*]Atom) void;

	pub const loadColors = iemgui_all_loadcolors;
	extern fn iemgui_all_loadcolors(*Self, bcol: *Atom, fcol: *Atom, lcol: *Atom) void;

	pub const setDrawFunctions = iemgui_setdrawfunctions;
	extern fn iemgui_setdrawfunctions(*Self, w: *const DrawFunctions) void;

	pub const save = iemgui_save;
	extern fn iemgui_save(*Self, srl: [*]*Symbol, bflcol: [*]*Symbol) void;

	/// Inform GUIs that glist's zoom is about to change.  The glist will
	/// take care of x,y locations but we have to adjust width and height.
	pub const zoom = iemgui_zoom;
	extern fn iemgui_zoom(*Self, zoom: Float) void;

	/// When creating a new GUI from menu onto a zoomed canvas, pretend to
	/// change the canvas's zoom so we'll get properly sized
	pub const newZoom = iemgui_newzoom;
	extern fn iemgui_newzoom(*Self) void;

	pub const properties = iemgui_properties;
	extern fn iemgui_properties(*Self, srl: [*]*pd.Symbol) void;

	pub fn size(self: *Self, x: *anyopaque) void {
		iemgui_size(x, self);
	}
	extern fn iemgui_size(*anyopaque, *Self) void;

	pub fn delta(self: *Self, x: *anyopaque, s: *Symbol, av: []Atom)
	void {
		iemgui_delta(x, self, s, @intCast(av.len), av.ptr);
	}
	extern fn iemgui_delta(*anyopaque, *Self, *Symbol, c_uint, [*]Atom) void;

	pub fn pos(self: *Self, x: *anyopaque, s: *Symbol, av: []Atom)
	void {
		iemgui_pos(x, self, s, @intCast(av.len), av.ptr);
	}
	extern fn iemgui_pos(*anyopaque, *Self, *Symbol, c_uint, [*]Atom) void;

	pub fn color(self: *Self, x: *anyopaque, s: *Symbol, av: []Atom)
	void {
		iemgui_color(x, self, s, @intCast(av.len), av.ptr);
	}
	extern fn iemgui_color(*anyopaque, *Self, *Symbol, c_uint, [*]Atom) void;

	pub fn send(self: *Self, x: *anyopaque, s: *Symbol) void {
		iemgui_send(x, self, s);
	}
	extern fn iemgui_send(*anyopaque, *Self, *Symbol) void;

	pub fn receive(self: *Self, x: *anyopaque, s: *Symbol) void {
		iemgui_receive(x, self, s);
	}
	extern fn iemgui_receive(*anyopaque, *Self, *Symbol) void;

	pub fn label(self: *Self, x: *anyopaque, s: *Symbol) void {
		iemgui_label(x, self, s);
	}
	extern fn iemgui_label(*anyopaque, *Self, *Symbol) void;

	pub fn labelPos(self: *Self, x: *anyopaque, s: *Symbol, av: []Atom) void {
		iemgui_label_pos(x, self, s, @intCast(av.len), av.ptr);
	}
	extern fn iemgui_label_pos(*anyopaque, *Self, *Symbol, c_uint, [*]Atom) void;

	pub fn labelFont(self: *Self, x: *anyopaque, s: *Symbol, av: []Atom) void {
		iemgui_label_font(x, self, s, @intCast(av.len), av.ptr);
	}
	extern fn iemgui_label_font(*anyopaque, *Self, *Symbol, c_uint, [*]Atom) void;

	pub fn doLabel(self: *Self, x: *anyopaque, s: *Symbol, senditup: c_int) void {
		iemgui_dolabel(x, self, s, senditup);
	}
	extern fn iemgui_dolabel(*anyopaque, *Self, *Symbol, c_uint) void;

	pub fn newDialog(
		self: *Self, x: *anyopaque,
		objname: [*:0]const u8,
		width: Float, width_min: Float,
		height: Float, height_min: Float,
		range_min: Float, range_max: Float, range_checkmode: c_int,
		mode: bool,
		mode_label0: [*:0]const u8, mode_label1: [*:0]const u8,
		canloadbang: bool, steady: c_int, number: c_int
	) void {
		iemgui_new_dialog(x, self, objname, width, width_min, height, height_min,
			range_min, range_max, range_checkmode, @intFromBool(mode),
			mode_label0, mode_label1, @intFromBool(canloadbang), steady, number);
	}
	extern fn iemgui_new_dialog(
		*anyopaque, *Self, [*:0]const u8,
		Float, Float, Float, Float, Float, Float, c_int, c_uint,
		[*:0]const u8, [*:0]const u8, canloadbang: c_uint,
		steady: c_int, number: c_int
	) void;

	pub fn setDialogAtoms(self: *Self, argv: []Atom) void {
		iemgui_setdialogatoms(self, @intCast(argv.len), argv.ptr);
	}
	extern fn iemgui_setdialogatoms(*Self, c_uint, [*]Atom) void;

	/// Returns a sendable/receivable bit mask.
	pub fn dialog(self: *Self, srl: []*Symbol, av: []Atom) u2 {
		return @intCast(iemgui_dialog(self, srl.ptr, @intCast(av.len), av.ptr));
	}
	extern fn iemgui_dialog(*Self, [*]*Symbol, c_uint, [*]Atom) c_uint;

	pub fn new(cls: *Class) Err!*Self {
		return iemgui_new(cls) orelse Err.GuiNew;
	}
	extern fn iemgui_new(*Class) ?*Self;
};

pub const displace = iemgui_displace;
extern fn iemgui_displace(*GObj, *GList, dx: c_int, dy: c_int) void;

pub fn setSelected(obj: *GObj, list: *GList, selected: bool) void {
	iemgui_select(obj, list, @intFromBool(selected));
}
extern fn iemgui_select(*GObj, *GList, c_uint) void;

pub const delete = iemgui_delete;
extern fn iemgui_delete(*GObj, *GList) void;

pub fn setVisible(obj: *GObj, list: *GList, vis: bool) void {
	iemgui_vis(obj, list, @intFromBool(vis));
}
extern fn iemgui_vis(*GObj, *GList, c_uint) void;
