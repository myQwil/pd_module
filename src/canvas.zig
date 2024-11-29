const pd = @import("pd.zig");

/// non-zero on success/true
const Bool = c_uint;
/// zero on success, otherwise represents an error
const Result = c_int;

pub const Error = GList.Error;

const Atom = pd.Atom;
const Float = pd.Float;
const Symbol = pd.Symbol;
const Word = pd.Word;

const GPointer = pd.GPointer;
const GStub = pd.GStub;
const GObj = pd.GObj;
const Object = pd.Object;
const BinBuf = pd.BinBuf;
const Clock = pd.Clock;
const Inlet = pd.Inlet;
const Outlet = pd.Outlet;
const Scalar = pd.Scalar;
const Pd = pd.Pd;

pub const io_width = 7;
pub const i_height = 3;
pub const o_height = 3;


// ----------------------------------- Array -----------------------------------
// -----------------------------------------------------------------------------
pub const Array = extern struct {
	len: c_uint,
	elemsize: c_uint,
	vec: [*]u8,
	templatesym: *Symbol,
	valid: c_uint,
	gp: GPointer,
	stub: *GStub,
};


// ---------------------------------- Editor -----------------------------------
// -----------------------------------------------------------------------------
const RText = opaque {};
const GuiConnect = opaque {};
const OutConnect = opaque {};

pub const UpdateHeader = extern struct {
	next: ?*UpdateHeader,
	flags: packed struct(u32) {
		array: bool,       // true if array, false if glist
		queued: bool,      // true if we're queued
		_padding: u30,
	},
};

const Selection = extern struct {
	what: *GObj,
	next: ?*Selection,
};

pub const Editor = extern struct {
	upd: UpdateHeader,           // update header structure
	updlist: *Selection,         // list of objects to update
	rtext: *RText,               // text responder linked list
	selection: *Selection,       // head of the selection list
	textedfor: *RText,           // the rtext if any that we are editing
	grab: *GObj,                 // object being "dragged"
	motionfn: GList.MotionFn,    // ... motion callback
	keyfn: GList.KeyFn,          // ... keypress callback
	connectbuf: *BinBuf,         // connections to deleted objects
	deleted: *BinBuf,            // last stuff we deleted
	guiconnect: *GuiConnect,     // GUI connection for filtering messages
	glist: *GList,               // glist which owns this
	xwas: c_int,                 // xpos on last mousedown or motion event
	ywas: c_int,                 // ypos, similarly
	selectline_index1: c_int,    // indices for the selected line if any
	selectline_outno: c_int,     // (only valid if e_selectedline is set)
	selectline_index2: c_int,
	selectline_inno: c_int,
	selectline_tag: *OutConnect,
	flags: packed struct(u32) {
		onmotion: OnMotion, // action to take on motion
		lastmoved: bool,    // true if mouse has moved since click
		textdirty: bool,    // one if e_textedfor has changed
		selectedline: bool, // one if a line is selected
		_padding: u26,
	},
	clock: *Clock,                   // clock to filter GUI move messages
	xnew: c_int,                     // xpos for next move event
	ynew: c_int,                     // ypos, similarly

	const OnMotion = enum(u3) {
		none,     // do nothing
		move,     // drag the selection around
		connect,  // make a connection
		region,   // selection region
		passout,  // send on to e_grab
		dragtext, // drag in text editor to alter selection
		resize,   // drag to resize
	};
};


// ----------------------------------- GList -----------------------------------
// -----------------------------------------------------------------------------
pub const CanvasEnvironment = opaque {};
const Tick = extern struct {    // where to put ticks on x or y axes
	point: Float,    // one point to draw a big tick at
	inc: Float,      // x or y increment per little tick
	lperb: c_int,    // little ticks per big; 0 if no ticks to draw
};

pub const GList = extern struct {
	const Self = @This();

	pub const Error = error {
		GListOpen,
	};
	const Err = Self.Error;

	pub const MotionFn = ?*const fn (*anyopaque, Float, Float, Float) callconv(.C) void;
	pub const KeyFn = ?*const fn (*anyopaque, *Symbol, Float) callconv(.C) void;

	/// header in case we're a glist
	obj: Object,
	/// the actual data
	list: *GObj,
	/// safe pointer handler
	stub: *GStub,
	/// incremented when pointers might be stale
	valid: c_int,
	/// parent glist, supercanvas, or 0 if none
	owner: *Self,
	/// width in pixels (on parent, if a graph)
	pixwidth: c_uint,
	/// height in pixels (on parent, if a graph)
	pixheight: c_uint,
	x1: Float,             // bounding rectangle in our own coordinates
	y1: Float,
	x2: Float,
	y2: Float,
	screenx1: c_int,       // screen coordinates when toplevel
	screeny1: c_int,
	screenx2: c_int,
	screeny2: c_int,
	xmargin: c_int,        // origin for GOP rectangle
	ymargin: c_int,
	xtick: Tick,           // ticks marking X values
	nxlabels: c_int,       // number of X coordinate labels
	xlabel: **Symbol,      // ... an array to hold them
	xlabely: Float,        // ... and their Y coordinates
	ytick: Tick,           // same as above for Y ticks and labels
	nylabels: c_int,
	ylabel: **Symbol,
	ylabelx: Float,
	editor: *Editor,       // editor structure when visible
	name: *Symbol,         // symbol bound here
	font: c_int,           // nominal font size in points, e.g., 10
	next: ?*Self,          // link in list of toplevels
	env: *CanvasEnvironment, // root canvases and abstractions only
	flags: packed struct(u32) {
		havewindow: bool,   // true if we own a window
		mapped: bool,       // true if, moreover, it's "mapped"
		dirty: bool,        // (root canvas only:) patch has changed
		loading: bool,      // am now loading from file
		willvis: bool,      // make me visible after loading
		edit: bool,         // edit mode
		isdeleting: bool,   // we're inside glist_delete -- hack!
		goprect: bool,      // draw rectangle for graph-on-parent
		isgraph: bool,      // show as graph on parent
		hidetext: bool,     // hide object-name + args when doing graph on parent
		private: bool,      // private flag used in x_scalar.c
		isclone: bool,      // exists as part of a clone object
		_padding: u20,
	},
	zoom: c_uint,          // zoom factor (integer zoom-in only)
	privatedata: *anyopaque, // private data

	pub const init = glist_init;
	extern fn glist_init(*Self) void;

	pub const add = glist_add;
	extern fn glist_add(*Self, *GObj) void;

	pub const clear = glist_clear;
	extern fn glist_clear(*Self) void;

	pub const canvas = glist_getcanvas;
	extern fn glist_getcanvas(*Self) *Self;

	pub fn isSelected(self: *Self, obj: *GObj) bool {
		return (glist_isselected(self, obj) != 0);
	}
	extern fn glist_isselected(*Self, *GObj) Bool;

	pub const select = glist_select;
	extern fn glist_select(*Self, *GObj) void;

	pub const deselect = glist_deselect;
	extern fn glist_deselect(*Self, *GObj) void;

	pub const noSelect = glist_noselect;
	extern fn glist_noselect(*Self) void;

	pub const selectAll = glist_selectall;
	extern fn glist_selectall(*Self) void;

	pub const delete = glist_delete;
	extern fn glist_delete(*Self, *GObj) void;

	/// Remake text buffer
	pub const retext = glist_retext;
	extern fn glist_retext(*Self, *Object) void;

	pub const grab = glist_grab;
	extern fn glist_grab(*Self, *GObj, MotionFn, KeyFn, xpos: c_int, ypos: c_int) void;

	pub fn isVisible(self: *Self) bool {
		return (glist_isvisible(self) != 0);
	}
	extern fn glist_isvisible(*Self) Bool;

	pub fn isTopLevel(self: *Self) bool {
		return (glist_istoplevel(self) != 0);
	}
	extern fn glist_istoplevel(*Self) Bool;

	/// Find the graph most recently added to this glist.
	/// If none exists, return null.
	pub const findGraph = glist_findgraph;
	extern fn glist_findgraph(*Self) ?*Self;

	pub fn font(self: *Self) u32 {
		return glist_getfont(self);
	}
	extern fn glist_getfont(*Self) c_uint;

	pub fn fontWidth(self: *Self) u32 {
		return glist_fontwidth(self);
	}
	extern fn glist_fontwidth(*Self) c_uint;

	pub fn fontHeight(self: *Self) u32 {
		return glist_fontheight(self);
	}
	extern fn glist_fontheight(*Self) c_uint;

	pub fn zoom(self: *Self) u32 {
		glist_getzoom(self);
	}
	extern fn glist_getzoom(*Self) c_uint;

	pub const sort = glist_sort;
	extern fn glist_sort(*Self) void;

	pub const read = glist_read;
	extern fn glist_read(*Self, filename: *Symbol, format: *Symbol) void;

	pub const mergeFile = glist_mergefile;
	extern fn glist_mergefile(*Self, filename: *Symbol, format: *Symbol) void;

	pub const pixelsToX = glist_pixelstox;
	extern fn glist_pixelstox(*Self, xpix: Float) Float;

	pub const pixelsToY = glist_pixelstoy;
	extern fn glist_pixelstoy(*Self, ypix: Float) Float;

	pub const xToPixels = glist_xtopixels;
	extern fn glist_xtopixels(*Self, xval: Float) Float;

	pub const yToPixels = glist_ytopixels;
	extern fn glist_ytopixels(*Self, yval: Float) Float;

	pub const dpixToDx = glist_dpixtodx;
	extern fn glist_dpixtodx(*Self, dxpix: Float) Float;

	pub const dpixToDy = glist_dpixtody;
	extern fn glist_dpixtody(*Self, dypix: Float) Float;

	pub const nextXY = glist_getnextxy;
	extern fn glist_getnextxy(*Self, xval: *c_int, yval: *c_int) void;

	/// Call `glist_addglist()` from a Pd message.
	pub fn gList(self: *Self, s: *Symbol, av: []Atom) void {
		glist_glist(self, s, @intCast(av.len), av.ptr);
	}
	extern fn glist_glist(*Self, *Symbol, c_uint, [*]Atom) void;

	/// Make a new glist and add it to this glist.
	/// It will appear as a "graph", not a text object.
	pub fn addGList(
		self: *Self, sym: *Symbol,
		x1: Float, y1: Float, x2: Float, y2: Float,
		px1: Float, py1: Float, px2: Float, py2: Float,
	) !*Self {
		return glist_addglist(self, sym, x1, y1, x2, y2, px1, py1, px2, py2)
			orelse error.GListAddGList;
	}
	extern fn glist_addglist(
		*Self, *Symbol, Float, Float, Float, Float, Float, Float, Float, Float) ?*Self;

	pub const arrayDialog = glist_arraydialog;
	extern fn glist_arraydialog(
		*Self, name: *Symbol, size: Float, saveit: Float, newgraph: Float) void;

	/// Write all "scalars" in a glist to a binbuf.
	pub fn writeToBinbuf(self: *Self, wholething: bool) !*BinBuf {
		return glist_writetobinbuf(self, @intFromBool(wholething))
			orelse error.GListWriteToBinBuf;
	}
	extern fn glist_writetobinbuf(*Self, c_uint) ?*BinBuf;

	pub fn isGraph(self: *Self) bool {
		return (glist_isgraph(self) != 0);
	}
	extern fn glist_isgraph(*Self) Bool;

	pub const redraw = glist_redraw;
	extern fn glist_redraw(*Self) void;

	/// Draw inlets and outlets for a text object or for a graph.
	pub fn drawIoFor(
		self: *Self, ob: *Object,
		first_time: bool,
		tag: [*:0]const u8,
		x1: i32, y1: i32,
		x2: i32, y2: i32,
	) void {
		glist_drawiofor(self, ob, @intFromBool(first_time), tag, x1, y1, x2, y2);
	}
	extern fn glist_drawiofor(
		*Self, *Object, c_uint, [*:0]const u8, c_int, c_int, c_int, c_int) void;

	pub fn eraseIoFor(self: *Self, ob: *Object, tag: [*:0]const u8) void {
		glist_eraseiofor(self, ob, tag);
	}
	extern fn glist_eraseiofor(*Self, *Object, [*:0]const u8) void;

	pub const createEditor = canvas_create_editor;
	extern fn canvas_create_editor(*Self) void;

	pub const destroyEditor = canvas_destroy_editor;
	extern fn canvas_destroy_editor(*Self) void;

	pub const deleteLinesForIo = canvas_deletelinesforio;
	extern fn canvas_deletelinesforio(*Self, text: *Object, *Inlet, *Outlet) void;

	pub const makeFilename = canvas_makefilename;
	extern fn canvas_makefilename(
		*const Self, file: [*:0]const u8, result: [*:0]u8, resultsize: c_int) void;

	pub const dir = canvas_getdir;
	extern fn canvas_getdir(*const Self) *Symbol;

	pub const dataProperties = canvas_dataproperties;
	extern fn canvas_dataproperties(*Self, *Scalar, *BinBuf) void;

	/// Utility function to read a file, looking first down the canvas's search
	/// path (set with "declare" objects in the patch and recursively in calling
	/// patches), then down the system one.  The filename is the concatenation of
	/// "name" and "ext".  "Name" may be absolute, or may be relative with
	/// slashes.  If anything can be opened, the true directory
	/// is put in the buffer dirresult (provided by caller), which should
	/// be "size" bytes.  The "nameresult" pointer will be set somewhere in
	/// the interior of "dirresult" and will give the file basename (with
	/// slashes trimmed).  If "bin" is set a 'binary' open is
	/// attempted, otherwise ASCII (this only matters on Microsoft.)
	/// If "x" is zero, the file is sought in the directory "." or in the
	/// global path.
	pub fn open(
		self: *const Self,
		name: [*:0]const u8, ext: [*:0]const u8,
		dirresult: [*:0]u8, nameresult: *[*]u8,
		size: u32, bin: bool,
	) Err!void {
		if (canvas_open(self, name, ext, dirresult, nameresult,
			size, @intFromBool(bin)) < 0)
		{
			return Err.GListOpen;
		}
	}
	extern fn canvas_open(*const Self, [*:0]const u8, [*:0]const u8,
		[*:0]u8, *[*]u8, c_uint, Bool) Result;

	pub const sampleRate = canvas_getsr;
	extern fn canvas_getsr(*Self) Float;

	pub fn signalLength(self: *Self) u32 {
		return canvas_getsignallength(self);
	}
	extern fn canvas_getsignallength(*Self) c_uint;

	pub fn setArgs(av: []const Atom) void {
		canvas_setargs(@intCast(av.len), av.ptr);
	}
	extern fn canvas_setargs(c_uint, [*]const Atom) void;

	pub fn args() []Atom {
		var ac: c_uint = undefined;
		var av: [*]Atom = undefined;
		canvas_getargs(&ac, &av);
		return av[0..ac];
	}
	extern fn canvas_getargs(*c_uint, *[*]Atom) void;

	pub fn undoSetState(
		self: *Self, x: *Pd, s: *Symbol,
		undo: []Atom, redo: []Atom,
	) void {
		pd_undo_set_objectstate(self, x, s,
			@intCast(undo.len), undo.ptr, @intCast(redo.len), redo.ptr);
	}
	extern fn pd_undo_set_objectstate(
		*Self, *Pd, *Symbol, c_uint, [*]Atom, c_uint, [*]Atom) void;
};

pub const currentCanvas = canvas_getcurrent;
extern fn canvas_getcurrent() ?*GList;

// --------------------------------- LoadBang ----------------------------------
// -----------------------------------------------------------------------------
pub const LoadBang = enum(u2) {
	load,
	init,
	close,
};


// --------------------------------- Template ----------------------------------
// -----------------------------------------------------------------------------
pub const GTemplate = opaque {};
pub const DataSlot = extern struct {
	type: c_int,
	name: *Symbol,
	arraytemplate: *Symbol,
};

pub const Template = extern struct {
	pdobj: Pd,
	list: *GTemplate,
	sym: *Symbol,
	n: c_int,
	vec: *DataSlot,
	next: ?*Template,
};


// ---------------------------------- Widgets ----------------------------------
// -----------------------------------------------------------------------------
pub const GetRectFn = ?*const fn (
	*GObj, *GList, *c_int, *c_int, *c_int, *c_int) callconv(.C) void;
pub const ClickFn = ?*const fn (
	*GObj, *GList, c_int, c_int, c_int, c_int, c_int, c_int) callconv(.C) c_int;
pub const DisplaceFn = ?*const fn (*GObj, *GList, c_int, c_int) callconv(.C) void;
pub const SelectFn = ?*const fn (*GObj, *GList, c_int) callconv(.C) void;
pub const ActivateFn = ?*const fn (*GObj, *GList, c_int) callconv(.C) void;
pub const DeleteFn = ?*const fn (*GObj, *GList) callconv(.C) void;
pub const VisFn = ?*const fn (*GObj, *GList, c_int) callconv(.C) void;

pub const WidgetBehavior = extern struct {
	getrect: GetRectFn = null,
	displace: DisplaceFn = null,
	select: SelectFn = null,
	activate: ActivateFn = null,
	delete: DeleteFn = null,
	vis: VisFn = null,
	click: ClickFn = null,
};

pub const parent = struct {
	const Self = @This();

	pub const GetRectFn = ?*const fn (*GObj, *GList, *Word, *Template,
		Float, Float, *c_int, *c_int, *c_int, *c_int) callconv(.C) void;
	pub const DisplaceFn = ?*const fn (*GObj, *GList, *Word, *Template,
		Float, Float, c_int, c_int) callconv(.C) void;
	pub const SelectFn = ?*const fn (*GObj, *GList, *Word, *Template,
		Float, Float, c_int) callconv(.C) void;
	pub const ActivateFn = ?*const fn (*GObj, *GList, *Word, *Template,
		Float, Float, c_int) callconv(.C) void;
	pub const VisFn = ?*const fn (*GObj, *GList, *Word, *Template,
		Float, Float, c_int) callconv(.C) void;
	pub const ClickFn = ?*const fn (*GObj, *GList, *Word, *Template, *Scalar, *Array,
		Float, Float, c_int, c_int, c_int, c_int, c_int, c_int) callconv(.C) c_int;

	pub const WidgetBehavior = extern struct {
		getrect: Self.GetRectFn = null,
		displace: Self.DisplaceFn = null,
		select: Self.SelectFn = null,
		activate: Self.ActivateFn = null,
		vis: Self.VisFn = null,
		click: Self.ClickFn = null,
	};
};
