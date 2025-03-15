const pd = @import("pd.zig");

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
	flags: packed struct(c_uint) {
		/// true if array, false if glist
		array: bool,
		/// true if we're queued
		queued: bool,
		_unused: @Type(.{.int = .{
			.signedness = .unsigned, .bits = @bitSizeOf(c_uint) - 2,
		}}),
	},
};

const Selection = extern struct {
	what: *GObj,
	next: ?*Selection,
};

pub const Editor = extern struct {
	/// update header structure
	upd: UpdateHeader,
	/// list of objects to update
	updlist: *Selection,
	/// text responder linked list
	rtext: *RText,
	/// head of the selection list
	selection: *Selection,
	/// the rtext if any that we are editing
	textedfor: *RText,
	/// object being "dragged"
	grab: *GObj,
	/// motion callback
	motionfn: GList.MotionFn,
	/// keypress callback
	keyfn: GList.KeyFn,
	/// connections to deleted objects
	connectbuf: *BinBuf,
	/// last stuff we deleted
	deleted: *BinBuf,
	/// GUI connection for filtering messages
	guiconnect: *GuiConnect,
	/// glist which owns this
	glist: *GList,
	/// xpos on last mousedown or motion event
	xwas: c_int,
	/// ypos on last mousedown or motion event
	ywas: c_int,
	/// indices for the selected line if any
	selectline_index1: c_int,
	/// (only valid if e_selectedline is set)
	selectline_outno: c_int,
	selectline_index2: c_int,
	selectline_inno: c_int,
	selectline_tag: *OutConnect,
	flags: packed struct(c_uint) {
		/// action to take on motion
		onmotion: OnMotion,
		/// true if mouse has moved since click
		lastmoved: bool,
		/// one if e_textedfor has changed
		textdirty: bool,
		/// one if a line is selected
		selectedline: bool,
		_unused: @Type(.{.int = .{
			.signedness = .unsigned, .bits = @bitSizeOf(c_uint) - 6,
		}}),
	},
	/// clock to filter GUI move messages
	clock: *Clock,
	/// xpos for next move event
	xnew: c_int,
	/// ypos for next move event
	ynew: c_int,

	const OnMotion = enum(u3) {
		/// do nothing
		none,
		/// drag the selection around
		move,
		/// make a connection
		connect,
		/// selection region
		region,
		/// send on to e_grab
		passout,
		/// drag in text editor to alter selection
		dragtext,
		/// drag to resize
		resize,
	};
};


// ----------------------------------- GList -----------------------------------
// -----------------------------------------------------------------------------
/// where to put ticks on x or y axes
const Tick = extern struct {
	/// one point to draw a big tick at
	point: Float,
	/// x or y increment per little tick
	inc: Float,
	/// little ticks per big; 0 if no ticks to draw
	lperb: c_int,
};

pub const GList = extern struct {
	pub const Error = error {
		GListOpen,
	};

	pub const Environment = opaque {};

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
	owner: *GList,
	/// width in pixels (on parent, if a graph)
	pixwidth: c_uint,
	/// height in pixels (on parent, if a graph)
	pixheight: c_uint,
	/// bounding rectangle in our own coordinates (x1)
	x1: Float,
	/// bounding rectangle in our own coordinates (y1)
	y1: Float,
	/// bounding rectangle in our own coordinates (x2)
	x2: Float,
	/// bounding rectangle in our own coordinates (y2)
	y2: Float,
	/// screen coordinates when toplevel (x1)
	screenx1: c_int,
	/// screen coordinates when toplevel (y1)
	screeny1: c_int,
	/// screen coordinates when toplevel (x2)
	screenx2: c_int,
	/// screen coordinates when toplevel (y2)
	screeny2: c_int,
	/// X origin for GOP rectangle
	xmargin: c_int,
	/// Y origin for GOP rectangle
	ymargin: c_int,
	/// ticks marking X values
	xtick: Tick,
	/// number of X coordinate labels
	nxlabels: c_uint,
	/// array to hold X coordinate labels
	xlabel: [*]*Symbol,
	/// Y coordinate for X coordinate labels
	xlabely: Float,
	/// ticks marking Y values
	ytick: Tick,
	/// number of Y coordinate labels
	nylabels: c_uint,
	/// array to hold Y coordinate labels
	ylabel: [*]*Symbol,
	/// X coordinate for Y coordinate labels
	ylabelx: Float,
	/// editor structure when visible
	editor: *Editor,
	/// symbol bound here
	name: *Symbol,
	/// nominal font size in points, e.g., 10
	font: c_uint,
	/// link in list of toplevels
	next: ?*GList,
	/// root canvases and abstractions only
	env: *Environment,
	flags: packed struct(c_uint) {
		/// true if we own a window
		havewindow: bool,
		/// true if, moreover, it's "mapped"
		mapped: bool,
		/// (root canvas only:) patch has changed
		dirty: bool,
		/// am now loading from file
		loading: bool,
		/// make me visible after loading
		willvis: bool,
		/// edit mode
		edit: bool,
		/// we're inside glist_delete -- hack!
		isdeleting: bool,
		/// draw rectangle for graph-on-parent
		goprect: bool,
		/// show as graph on parent
		isgraph: bool,
		/// hide object-name + args when doing graph on parent
		hidetext: bool,
		/// private flag used in x_scalar.c
		private: bool,
		/// exists as part of a clone object
		isclone: bool,
		_unused: @Type(.{.int = .{
			.signedness = .unsigned, .bits = @bitSizeOf(c_uint) - 12,
		}}),
	},
	/// zoom factor (integer zoom-in only)
	zoom: c_uint,
	/// private data
	privatedata: *anyopaque,

	pub const init = glist_init;
	extern fn glist_init(*GList) void;

	pub const add = glist_add;
	extern fn glist_add(*GList, *GObj) void;

	pub const clear = glist_clear;
	extern fn glist_clear(*GList) void;

	pub const canvas = glist_getcanvas;
	extern fn glist_getcanvas(*GList) *GList;

	pub fn isSelected(self: *GList, obj: *GObj) bool {
		return (glist_isselected(self, obj) != 0);
	}
	extern fn glist_isselected(*GList, *GObj) c_uint;

	pub const select = glist_select;
	extern fn glist_select(*GList, *GObj) void;

	pub const deselect = glist_deselect;
	extern fn glist_deselect(*GList, *GObj) void;

	pub const noSelect = glist_noselect;
	extern fn glist_noselect(*GList) void;

	pub const selectAll = glist_selectall;
	extern fn glist_selectall(*GList) void;

	pub const delete = glist_delete;
	extern fn glist_delete(*GList, *GObj) void;

	/// Remake text buffer
	pub const retext = glist_retext;
	extern fn glist_retext(*GList, *Object) void;

	pub const grab = glist_grab;
	extern fn glist_grab(*GList, *GObj, MotionFn, KeyFn, xpos: c_int, ypos: c_int) void;

	pub fn isVisible(self: *GList) bool {
		return (glist_isvisible(self) != 0);
	}
	extern fn glist_isvisible(*GList) c_uint;

	pub fn isTopLevel(self: *GList) bool {
		return (glist_istoplevel(self) != 0);
	}
	extern fn glist_istoplevel(*GList) c_uint;

	/// Find the graph most recently added to this glist.
	/// If none exists, return null.
	pub const findGraph = glist_findgraph;
	extern fn glist_findgraph(*GList) ?*GList;

	pub const getFont = glist_getfont;
	extern fn glist_getfont(*GList) c_uint;

	pub const fontWidth = glist_fontwidth;
	extern fn glist_fontwidth(*GList) c_uint;

	pub const fontHeight = glist_fontheight;
	extern fn glist_fontheight(*GList) c_uint;

	pub const getZoom = glist_getzoom;
	extern fn glist_getzoom(*GList) c_uint;

	pub const sort = glist_sort;
	extern fn glist_sort(*GList) void;

	pub const read = glist_read;
	extern fn glist_read(*GList, filename: *Symbol, format: *Symbol) void;

	pub const mergeFile = glist_mergefile;
	extern fn glist_mergefile(*GList, filename: *Symbol, format: *Symbol) void;

	pub const pixelsToX = glist_pixelstox;
	extern fn glist_pixelstox(*GList, xpix: Float) Float;

	pub const pixelsToY = glist_pixelstoy;
	extern fn glist_pixelstoy(*GList, ypix: Float) Float;

	pub const xToPixels = glist_xtopixels;
	extern fn glist_xtopixels(*GList, xval: Float) Float;

	pub const yToPixels = glist_ytopixels;
	extern fn glist_ytopixels(*GList, yval: Float) Float;

	pub const dpixToDx = glist_dpixtodx;
	extern fn glist_dpixtodx(*GList, dxpix: Float) Float;

	pub const dpixToDy = glist_dpixtody;
	extern fn glist_dpixtody(*GList, dypix: Float) Float;

	pub const nextXY = glist_getnextxy;
	extern fn glist_getnextxy(*GList, xval: *c_int, yval: *c_int) void;

	/// Call `glist_addglist()` from a Pd message.
	pub fn gList(self: *GList, s: *Symbol, av: []Atom) void {
		glist_glist(self, s, @intCast(av.len), av.ptr);
	}
	extern fn glist_glist(*GList, *Symbol, c_uint, [*]Atom) void;

	/// Make a new glist and add it to this glist.
	/// It will appear as a "graph", not a text object.
	pub fn addGList(
		self: *GList, sym: *Symbol,
		x1: Float, y1: Float, x2: Float, y2: Float,
		px1: Float, py1: Float, px2: Float, py2: Float,
	) !*GList {
		return glist_addglist(self, sym, x1, y1, x2, y2, px1, py1, px2, py2)
			orelse error.GListAddGList;
	}
	extern fn glist_addglist(
		*GList, *Symbol, Float, Float, Float, Float, Float, Float, Float, Float) ?*GList;

	pub const arrayDialog = glist_arraydialog;
	extern fn glist_arraydialog(
		*GList, name: *Symbol, size: Float, saveit: Float, newgraph: Float) void;

	/// Write all "scalars" in a glist to a binbuf.
	pub fn writeToBinbuf(self: *GList, wholething: bool) !*BinBuf {
		return glist_writetobinbuf(self, @intFromBool(wholething))
			orelse error.GListWriteToBinBuf;
	}
	extern fn glist_writetobinbuf(*GList, c_uint) ?*BinBuf;

	pub fn isGraph(self: *GList) bool {
		return (glist_isgraph(self) != 0);
	}
	extern fn glist_isgraph(*GList) c_uint;

	pub const redraw = glist_redraw;
	extern fn glist_redraw(*GList) void;

	/// Draw inlets and outlets for a text object or for a graph.
	pub fn drawIoFor(
		self: *GList,
		ob: *Object,
		first_time: bool,
		tag: [*:0]const u8,
		x1: c_int,
		y1: c_int,
		x2: c_int,
		y2: c_int,
	) void {
		glist_drawiofor(self, ob, @intFromBool(first_time), tag, x1, y1, x2, y2);
	}
	extern fn glist_drawiofor(
		*GList, *Object, c_uint, [*:0]const u8, c_int, c_int, c_int, c_int) void;

	pub fn eraseIoFor(self: *GList, ob: *Object, tag: [*:0]const u8) void {
		glist_eraseiofor(self, ob, tag);
	}
	extern fn glist_eraseiofor(*GList, *Object, [*:0]const u8) void;

	pub const createEditor = canvas_create_editor;
	extern fn canvas_create_editor(*GList) void;

	pub const destroyEditor = canvas_destroy_editor;
	extern fn canvas_destroy_editor(*GList) void;

	pub const deleteLinesForIo = canvas_deletelinesforio;
	extern fn canvas_deletelinesforio(*GList, text: *Object, *Inlet, *Outlet) void;

	pub const makeFilename = canvas_makefilename;
	extern fn canvas_makefilename(
		*const GList, file: [*:0]const u8, result: [*:0]u8, resultsize: c_int) void;

	pub const dir = canvas_getdir;
	extern fn canvas_getdir(*const GList) *Symbol;

	pub const dataProperties = canvas_dataproperties;
	extern fn canvas_dataproperties(*GList, *Scalar, *BinBuf) void;

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
		self: *const GList,
		name: [*:0]const u8,
		ext: [*:0]const u8,
		dirresult: [*:0]u8,
		nameresult: *[*:0]u8,
		size: c_uint,
		bin: bool,
	) Error!void {
		if (canvas_open(self, name, ext, dirresult, nameresult,
			size, @intFromBool(bin)) < 0)
		{
			return Error.GListOpen;
		}
	}
	extern fn canvas_open(*const GList, [*:0]const u8, [*:0]const u8,
		[*:0]u8, *[*:0]u8, c_uint, c_uint) c_int;

	pub const sampleRate = canvas_getsr;
	extern fn canvas_getsr(*GList) Float;

	pub const signalLength = canvas_getsignallength;
	extern fn canvas_getsignallength(*GList) c_uint;

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
		self: *GList, x: *Pd, s: *Symbol,
		undo: []Atom, redo: []Atom,
	) void {
		pd_undo_set_objectstate(self, x, s,
			@intCast(undo.len), undo.ptr, @intCast(redo.len), redo.ptr);
	}
	extern fn pd_undo_set_objectstate(
		*GList, *Pd, *Symbol, c_uint, [*]Atom, c_uint, [*]Atom) void;

	pub const current = canvas_getcurrent;
	extern fn canvas_getcurrent() ?*GList;

	pub const environment = canvas_getenv;
	extern fn canvas_getenv(*GList) *Environment;
};


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
		getrect: parent.GetRectFn = null,
		displace: parent.DisplaceFn = null,
		select: parent.SelectFn = null,
		activate: parent.ActivateFn = null,
		vis: parent.VisFn = null,
		click: parent.ClickFn = null,
	};
};
