// -- Shadow DOM ---------------------------------------------------------------

const shadow_dom = new Map();   // id -> node
shadow_dom.set('graft', document.body);

// -- Terminal -----------------------------------------------------------------

function open_term(name, theme) {
    node = document.getElementById(name);
    fito = new FitAddon.FitAddon();
    term = new Terminal({
        theme: theme,
        fontSize: 14,
    });
    term.loadAddon(fito);
    term.open(node);
    fito.fit();
    
    return term;
}

const ostr_nl = '\r\n';
const ostr_style_non = '\x1B[0m';
const ostr_style_err = '\x1B[1;3;31m';

// -- Streams ------------------------------------------------------------------

var repl_xterm = open_term('repl', { background: '#112' });
var ostr_xterm = open_term('ostr', { background: '#224' });

ostr_xterm.write('Are we consing yet?' + ostr_nl);
repl_xterm.write("Loading Web Embeddable Common Lisp...");

var read_promise = null;
var read_promise_var = null;

repl_xterm.onData(self => { handle_repl_key(self) });
ostr_xterm.onData(self => { handle_ostr_key(self) });

function handle_repl_key(self) {
    if (read_promise_var == repl_xterm) {
        read_promise(self);
    }
}

function handle_ostr_key(self) {
    if (read_promise_var == ostr_xterm) {
        read_promise(self);
    }
}

// -- Example Shaders ----------------------------------------------------------
const vshader_text = `#version 300 es
precision mediump float;
in vec2 position;
void main() { gl_Position = vec4(position.x, position.y, 0.0, 1.0); }`;


const fshader_text = `#version 300 es
precision mediump float;
out vec4 color;
void main () { color = vec4(1.0,0.3,0.3,1.0); }`;

// -- WASM ---------------------------------------------------------------------

function ecl_stdout(msg) {
    ostr_xterm.write(ostr_style_non);
    ostr_xterm.write(msg);
}

function ecl_stderr(msg) {
    ostr_xterm.write(ostr_style_err);
    ostr_xterm.write(msg);
}

// ecl_stdout('stdout test: Hello World!' + ostr_nl);
// ecl_stderr('stderr test: Hello World!' + ostr_nl);

Module['print']    = function(text) { ecl_stdout(text+ostr_nl) };
Module['printErr'] = function(text) { ecl_stderr(text+ostr_nl) };
