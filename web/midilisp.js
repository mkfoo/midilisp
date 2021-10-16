const editorConfig = { lineNumbers: true };

class Midilisp {
    constructor() {
        const text = document.getElementById("input-text");
        this.editor = CodeMirror.fromTextArea(text, editorConfig);
        this.output = document.getElementById("output-text");
        this.filename = document.getElementById("filename-text");
        this.dLink = document.getElementById("dl-link");
        this.saveBtn = document.getElementById("save-btn");
        this.helpBtn = document.getElementById("help-btn");
        this.helpText = document.getElementById("help-text");
        this.loadWASM().catch((err) => {
            this.logOutput(`Err: ${err.message}`);
        });
    }

    async loadWASM() {
        this.logOutput("Loading WASM module...");
        this.mod = await WebAssembly.compileStreaming(fetch("midilisp.wasm"));
        this.logOutput("Ok");

        const runBtn = document.getElementById("run-btn");

        runBtn.addEventListener("click", (_e) => {
            this.run()
                .catch((err) => {
                    this.logOutput(`Err: ${err.message}`);
                });
        });

        this.saveBtn.addEventListener("click", (_e) => {
            this.dLink.click();
        });

        this.helpBtn.addEventListener("click", (e) => {
            e.preventDefault();

            if (this.helpText.hidden) {
                this.helpBtn.textContent = "Hide help";
                this.helpText.hidden = false;
            } else {
                this.helpBtn.textContent = "Help";
                this.helpText.hidden = true;
            }
        });
    }

    async run() {
        const src = this.editor.getValue();

        if (src && this.mod) {
            this.inst = await WebAssembly.instantiate(this.mod);
            const [ptr, len] = this.exportString(src);
            const result = this.inst.exports.run(ptr, len);

            if (this.inst.exports.isOk(result)) {
                const ptr = this.inst.exports.unwrap(result);
                const bytes = this.importBytes(ptr);
                const msg = this.serveFile(bytes);
                this.logOutput(msg);
            } else {
                const ptr = this.inst.exports.unwrap(result);
                const msg = this.importString(ptr);
                this.logOutput(msg);
            }
            
            this.inst = null;
        }
    }

    exportBytes(bytes) {
        const len = bytes.length;
        const ptr = this.inst.exports.malloc(len);
        const mem = new Uint8Array(this.inst.exports.memory.buffer);
        mem.set(bytes, ptr);
        return [ptr, len];
    }

    exportString(str) {
        const bytes = new TextEncoder("utf-8").encode(str);
        return this.exportBytes(bytes);
    }

    importBytes(ptrToVec) {
        const ptr = this.inst.exports.getVecPtr(ptrToVec);
        const len = this.inst.exports.getVecLen(ptrToVec);
        const mem = new Uint8Array(this.inst.exports.memory.buffer);
        return mem.slice(ptr, ptr + len);
    }

    importString(ptr) {
        const bytes = this.importBytes(ptr);
        return new TextDecoder("utf-8").decode(bytes);
    }

    serveFile(bytes) {
        let filename = this.filename.value || "Untitled.mid";
        const lower = filename.toLowerCase();

        if (!(lower.endsWith(".mid") || lower.endsWith(".midi"))) {
            filename += ".mid";
        }

        URL.revokeObjectURL(this.dLink.href);
        this.dLink.href = URL.createObjectURL(
            new File([bytes], filename, { type: "audio/midi" }),
        );
        this.dLink.download = filename;
        this.saveBtn.hidden = false;
        return `Ok: ${this.getLen(bytes.length)} written to ${filename}`;
    }

    logOutput(str) {
        this.output.value += `${str}\n`;
        this.output.scrollBy(0, 10000);
    }

    getLen(n) {
        if (n < 1000) {
            return `${n} bytes`;
        } else if (n < 1000 ** 2) {
            return `${(n / 1000).toFixed(1)} kB`;
        } else if (n < 1000 ** 3) {
            return `${(n / 1000 ** 2).toFixed(1)} MB`;
        } else {
            return `${(n / 1000 ** 3).toFixed(1)} GB`;
        }
    }
}

export default Midilisp;
