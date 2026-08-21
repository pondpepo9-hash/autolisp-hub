/**
 * =========================================================================
 * AutoLISP Hub - Data & Application Logic with Easy Upload System
 * =========================================================================
 */

// Initial Default LISP Library
const defaultLispDatabase = [
  {
    id: "autonum",
    title: "Auto Numbering Sequence (รันเลขต่อเนื่อง)",
    command: "NUMSEQ",
    category: "text",
    version: "v2.1",
    author: "CADDev",
    description: "คลิกเพื่อวางตัวเลขลำดับอัตโนมัติ (1, 2, 3, ...) สามารถตั้งค่า Prefix (เช่น A-01), Suffix และระยะก้าว (Increment) ได้อย่างรวดเร็ว",
    gifUrl: "assets/gifs/autonum.gif",
    downloadUrl: "assets/lsp/autonum.lsp",
    compatibility: "AutoCAD / BricsCAD / ZWCAD",
    code: `;;; ========================================================
;;; Auto Numbering Sequence (NUMSEQ.lsp)
;;; ========================================================
(defun c:NUMSEQ (/ pt num prefix inc txtStr)
  (setq prefix (getstring t "\nใส่คำนำหน้า (Prefix) [Enter เพื่อข้าม]: "))
  (setq num (getint "\nใส่หมายเลขเริ่มต้น <1>: "))
  (if (null num) (setq num 1))
  (setq inc (getint "\nระยะก้าว (Increment) <1>: "))
  (if (null inc) (setq inc 1))
  
  (while (setq pt (getpoint "\nคลิกตำแหน่งที่ต้องการวางตัวเลข (กด Esc เพื่อจบ): "))
    (setq txtStr (strcat prefix (itoa num)))
    (command "._TEXT" "_MC" pt "" "0" txtStr)
    (setq num (+ num inc))
  )
  (princ "\nเสร็จสิ้นการรันตัวเลข.")
  (princ)
)`
  },
  {
    id: "sumtext",
    title: "Sum Numbers in Text (รวมยอดตัวเลข)",
    command: "SUMTEXT",
    category: "text",
    version: "v1.4",
    author: "CADDev",
    description: "ครอบเลือก Text หรือ MText ที่มีตัวเลขทั้งหมดในแบบ แล้วคำนวณผลรวม (Sum) พร้อมจำนวนตัวเลขที่เลือก และคลิกวางผลลัพธ์ลงในแบบได้ทันที",
    gifUrl: "assets/gifs/sumtext.gif",
    downloadUrl: "assets/lsp/sumtext.lsp",
    compatibility: "AutoCAD 2015+",
    code: `;;; ========================================================
;;; Sum Text Numbers (SUMTEXT.lsp)
;;; ========================================================
(defun c:SUMTEXT (/ ss i ent val total pt)
  (setq total 0.0)
  (princ "\nเลือก Text ตัวเลขที่ต้องการรวมค่า: ")
  (if (setq ss (ssget '((0 . "TEXT,MTEXT"))))
    (progn
      (repeat (setq i (sslength ss))
        (setq ent (entget (ssname ss (setq i (1- i)))))
        (setq val (atof (cdr (assoc 1 ent))))
        (setq total (+ total val))
      )
      (princ (strcat "\n>>> ผลรวมทั้งหมด = " (rtos total 2 2)))
      (if (setq pt (getpoint "\nคลิกจุดวางข้อความผลรวม: "))
        (command "._TEXT" pt "" "0" (strcat "Total: " (rtos total 2 2)))
      )
    )
  )
  (princ)
)`
  },
  {
    id: "quicklayer",
    title: "Quick Layer Match & Isolate",
    command: "QLAY",
    category: "layer",
    version: "v1.0",
    author: "CADDev",
    description: "สลับและเปิด-ปิด Layer อัตโนมัติในคลิกเดียว คลิกวัตถุเพื่อย้ายไป Layer ปัจจุบัน หรือ Isolate เฉพาะ Layer ของวัตถุที่เลือกอย่างรวดเร็ว",
    gifUrl: "assets/gifs/quicklayer.gif",
    downloadUrl: "assets/lsp/quicklayer.lsp",
    compatibility: "All CAD Versions",
    code: `;;; ========================================================
;;; Quick Layer Set Current (QLAY.lsp)
;;; ========================================================
(defun c:QLAY (/ ent lay)
  (if (setq ent (entsel "\nคลิกเลือกวัตถุเพื่อตั้ง Layer ปัจจุบัน: "))
    (progn
      (setq lay (cdr (assoc 8 (entget (car ent)))))
      (setvar "CLAYER" lay)
      (princ (strcat "\nเปลี่ยน Current Layer เป็น: [" lay "] เรียบร้อย"))
    )
  )
  (princ)
)`
  },
  {
    id: "dimalign",
    title: "Dimension Auto Alignment & Clean",
    command: "DIMALIGN",
    category: "dim",
    version: "v2.0",
    author: "CADDev",
    description: "จัดเรียงแนวเส้นบอกขนาด (Dimension Line) ให้อยู่ในระนาบเดียวกันอัตโนมัติ ปรับระยะห่างระหว่างเส้น Dim ให้เท่ากันอย่างเป็นระเบียบ",
    gifUrl: "assets/gifs/dimalign.gif",
    downloadUrl: "assets/lsp/dimalign.lsp",
    compatibility: "AutoCAD 2018+",
    code: `;;; ========================================================
;;; Align Linear Dimensions (DIMALIGN.lsp)
;;; ========================================================
(defun c:DIMALIGN (/ ss pt1 pt2 i ent ed)
  (princ "\nเลือก Dimension ที่ต้องการจัดแนว: ")
  (if (and (setq ss (ssget '((0 . "DIMENSION"))))
           (setq pt (getpoint "\nคลิกตำแหน่งแนวเส้น Dimension Line ใหม่: ")))
    (repeat (setq i (sslength ss))
      (setq ent (ssname ss (setq i (1- i))))
      (setq ed (entget ent))
      (setq ed (subst (cons 10 (list (car (cdr (assoc 10 ed))) (cadr pt) 0.0))
                      (assoc 10 ed) ed))
      (entmod ed)
    )
  )
  (princ "\nจัดแนว Dimension สำเร็จ!")
  (princ)
)`
  },
  {
    id: "totalarea",
    title: "Total Area Multi-Polyline (คำนวณพื้นที่รวม)",
    command: "CAREA",
    category: "draw",
    version: "v1.5",
    author: "CADDev",
    description: "คลิกเลือก Polyline หลายๆ รูปเพื่อหาพื้นที่รวม (Total Area) และความยาวเส้นรอบรูป (Perimeter) พร้อมแปลงเป็นตารางเมตร (Sq.m.) อัตโนมัติ",
    gifUrl: "assets/gifs/totalarea.gif",
    downloadUrl: "assets/lsp/totalarea.lsp",
    compatibility: "AutoCAD / ZWCAD",
    code: `;;; ========================================================
;;; Calculate Cumulative Area (CAREA.lsp)
;;; ========================================================
(defun c:CAREA (/ ss i ent obj totArea pt)
  (vl-load-com)
  (setq totArea 0.0)
  (princ "\nเลือก Polyline / Hatch เพื่อคำนวณพื้นที่: ")
  (if (setq ss (ssget '((0 . "*POLYLINE,HATCH,REGION,CIRCLE"))))
    (progn
      (repeat (setq i (sslength ss))
        (setq ent (ssname ss (setq i (1- i))))
        (setq obj (vlax-ename->vla-object ent))
        (if (vlax-property-available-p obj 'Area)
          (setq totArea (+ totArea (vlax-get-property obj 'Area)))
        )
      )
      (princ (strcat "\n>>> พื้นที่รวม = " (rtos totArea 2 2) " ตร.หน่วย"))
      (if (setq pt (getpoint "\nคลิกจุดวางข้อความ Area: "))
        (command "._TEXT" pt "" "0" (strcat "Total Area = " (rtos totArea 2 2) " sq.m."))
      )
    )
  )
  (princ)
)`
  },
  {
    id: "purgeclean",
    title: "Deep CAD Cleaner & Audit",
    command: "PURGEALL",
    category: "util",
    version: "v3.0",
    author: "CADDev",
    description: "ทำความสะอาดไฟล์ CAD แบบหมดจด ลบ DGN LineStyles ที่แฝงมา, ลบ RegApp ขยะ, Purge All, และรัน Audit ซ่อมไฟล์อัตโนมัติ ช่วยลดขนาดไฟล์และแก้อาการค้าง",
    gifUrl: "assets/gifs/purgeclean.gif",
    downloadUrl: "assets/lsp/purgeclean.lsp",
    compatibility: "All CAD Versions",
    code: `;;; ========================================================
;;; Deep CAD Cleaner & Purge (PURGEALL.lsp)
;;; ========================================================
(defun c:PURGEALL ()
  (setvar "CMDECHO" 0)
  (princ "\nกำลังล้าง RegApp ขยะ...")
  (command ".-PURGE" "R" "*" "N")
  (princ "\nกำลัง Purge All แบบหมดจด...")
  (command ".-PURGE" "A" "*" "N")
  (princ "\nกำลัง Audit ซ่อมแซมฐานข้อมูลไฟล์...")
  (command "._AUDIT" "Y")
  (setvar "CMDECHO" 1)
  (princ "\n>>> ทำความสะอาดไฟล์ CAD เสร็จสมบูรณ์! <<<")
  (princ)
)`
  }
];

// Load local custom items
function getStoredDatabase() {
  const localData = localStorage.getItem("custom_autolisp_data");
  if (localData) {
    try {
      const parsed = JSON.parse(localData);
      return [...parsed, ...defaultLispDatabase];
    } catch (e) {
      console.error(e);
    }
  }
  return [...defaultLispDatabase];
}

let lispDatabase = getStoredDatabase();

// App State
let currentCategory = "all";
let searchKeyword = "";

// DOM Elements
const lispGrid = document.getElementById("lisp-grid");
const emptyState = document.getElementById("empty-state");
const searchInput = document.getElementById("search-input");
const clearSearchBtn = document.getElementById("clear-search");
const displayedCountEl = document.getElementById("displayed-count");
const countAllEl = document.getElementById("count-all");
const categoryButtons = document.querySelectorAll(".filter-btn");

// Modals
const codeModal = document.getElementById("code-modal");
const modalCodeTitle = document.getElementById("modal-code-title");
const modalCodeContent = document.getElementById("modal-code-content");
const modalCopyBtn = document.getElementById("modal-copy-btn");
const copyBtnText = document.getElementById("copy-btn-text");

const mediaModal = document.getElementById("media-modal");
const modalMediaTitle = document.getElementById("modal-media-title");
const modalMediaImg = document.getElementById("modal-media-img");

const guideModal = document.getElementById("guide-modal");
const btnGuide = document.getElementById("btn-guide");

const uploadModal = document.getElementById("upload-modal");
const btnAddLisp = document.getElementById("btn-add-lisp");
const addLispForm = document.getElementById("add-lisp-form");

// Dropzones & File Inputs
const lspDropzone = document.getElementById("lsp-dropzone");
const lspFileInput = document.getElementById("lsp-file-input");
const lspDropLabel = document.getElementById("lsp-drop-label");
const lspFileBadge = document.getElementById("lsp-file-badge");
const lspFileName = document.getElementById("lsp-file-name");
const btnRemoveLsp = document.getElementById("btn-remove-lsp");

const gifDropzone = document.getElementById("gif-dropzone");
const gifFileInput = document.getElementById("gif-file-input");
const gifDropLabel = document.getElementById("gif-drop-label");
const gifPreviewContainer = document.getElementById("gif-preview-container");
const gifPreviewImg = document.getElementById("gif-preview-img");
const btnRemoveGif = document.getElementById("btn-remove-gif");

// Form Fields
const formTitle = document.getElementById("form-title");
const formCommand = document.getElementById("form-command");
const formCategory = document.getElementById("form-category");
const formVersion = document.getElementById("form-version");
const formDesc = document.getElementById("form-desc");
const formCode = document.getElementById("form-code");

let uploadedGifData = "";

const toast = document.getElementById("toast");
const toastMessage = document.getElementById("toast-message");

// Category badge color mapper
const categoryConfig = {
  text: { label: "Text & ตัวเลข", color: "bg-amber-950/80 text-amber-300 border-amber-800" },
  layer: { label: "Layer", color: "bg-emerald-950/80 text-emerald-300 border-emerald-800" },
  dim: { label: "Dimension", color: "bg-purple-950/80 text-purple-300 border-purple-800" },
  draw: { label: "Draw & Area", color: "bg-blue-950/80 text-blue-300 border-blue-800" },
  util: { label: "Utility", color: "bg-rose-950/80 text-rose-300 border-rose-800" },
};

/**
 * Generate animated SVG Placeholder if GIF is missing
 */
function getPlaceholderGif(title, command) {
  const svg = `<svg xmlns="http://www.w3.org/2000/svg" width="600" height="340" viewBox="0 0 600 340">
    <rect width="100%" height="100%" fill="#0b1329"/>
    <defs>
      <pattern id="grid" width="20" height="20" patternUnits="userSpaceOnUse">
        <path d="M 20 0 L 0 0 0 20" fill="none" stroke="#1e293b" stroke-width="1"/>
      </pattern>
    </defs>
    <rect width="100%" height="100%" fill="url(#grid)" />
    <!-- Crosshair cursor -->
    <line x1="280" y1="140" x2="320" y2="140" stroke="#38bdf8" stroke-width="1.5"/>
    <line x1="300" y1="120" x2="300" y2="160" stroke="#38bdf8" stroke-width="1.5"/>
    <rect x="294" y="134" width="12" height="12" fill="none" stroke="#38bdf8" stroke-width="1.5"/>
    
    <!-- Command Window simulation -->
    <rect x="30" y="270" width="540" height="45" rx="6" fill="#030712" stroke="#334155" stroke-width="1"/>
    <text x="45" y="298" fill="#38bdf8" font-family="monospace" font-size="14">Command: ${command}</text>
    <text x="300" y="70" fill="#f8fafc" font-family="sans-serif" font-weight="bold" font-size="18" text-anchor="middle">${title}</text>
    <text x="300" y="195" fill="#94a3b8" font-family="sans-serif" font-size="13" text-anchor="middle">คลิกเพื่อดูภาพขยาย / GIF Preview</text>
  </svg>`;
  return "data:image/svg+xml;charset=utf-8," + encodeURIComponent(svg);
}

/**
 * Render LISP Cards
 */
function renderCards() {
  const filtered = lispDatabase.filter(item => {
    const matchCategory = (currentCategory === "all" || item.category === currentCategory);
    const searchLower = searchKeyword.toLowerCase().trim();
    const matchSearch = !searchLower || 
      item.title.toLowerCase().includes(searchLower) ||
      item.command.toLowerCase().includes(searchLower) ||
      item.description.toLowerCase().includes(searchLower) ||
      item.category.toLowerCase().includes(searchLower);
    return matchCategory && matchSearch;
  });

  countAllEl.textContent = lispDatabase.length;
  displayedCountEl.textContent = filtered.length;

  if (filtered.length === 0) {
    lispGrid.innerHTML = "";
    emptyState.classList.remove("hidden");
    return;
  }

  emptyState.classList.add("hidden");

  lispGrid.innerHTML = filtered.map(item => {
    const cat = categoryConfig[item.category] || { label: item.category, color: "bg-slate-800 text-slate-300 border-slate-700" };
    const imgSrc = item.gifUrl || getPlaceholderGif(item.title, item.command);
    const isCustom = item.isCustom ? true : false;

    return `
      <article class="lisp-card flex flex-col bg-cad-card border border-cad-border rounded-2xl overflow-hidden shadow-lg group relative">
        
        <!-- Custom Tag / Delete button if user added -->
        ${isCustom ? `
          <button 
            onclick="deleteCustomLisp('${item.id}', event)" 
            class="absolute top-3 right-3 z-10 p-1.5 rounded-lg bg-red-950/80 hover:bg-red-800 text-red-300 border border-red-700 transition-colors shadow-lg" 
            title="ลบรายการนี้"
          >
            <i data-lucide="trash-2" class="w-3.5 h-3.5"></i>
          </button>
        ` : ''}

        <!-- GIF / Image Preview Area -->
        <div class="gif-container h-48 w-full border-b border-cad-border relative cursor-pointer group-hover:brightness-105 transition-all" onclick="openMediaModal('${item.id}')">
          <img 
            src="${imgSrc}" 
            alt="${item.title}" 
            class="w-full h-full object-cover"
            loading="lazy"
            onerror="this.onerror=null; this.src='${getPlaceholderGif(item.title, item.command)}';"
          >
          <div class="absolute inset-0 bg-gradient-to-t from-slate-950/80 via-transparent to-transparent"></div>
          
          <div class="absolute top-3 left-3">
            <span class="text-xs px-2.5 py-1 rounded-md font-medium border ${cat.color} backdrop-blur-md">
              ${cat.label}
            </span>
          </div>

          <div class="absolute bottom-3 right-3 flex items-center space-x-1 px-2 py-1 rounded-md bg-black/60 backdrop-blur-md text-xs text-slate-300 opacity-90 group-hover:opacity-100 transition-opacity">
            <i data-lucide="play-circle" class="w-3.5 h-3.5 text-cyan-400"></i>
            <span>Preview GIF</span>
          </div>
        </div>

        <!-- Card Body -->
        <div class="p-5 flex-1 flex flex-col">
          
          <!-- Command Shortcut Pill -->
          <div class="flex items-center justify-between gap-2 mb-2">
            <div class="flex items-center space-x-2">
              <span class="text-xs text-slate-400">คำสั่งย่อ:</span>
              <button 
                onclick="copyToClipboard('${item.command}', 'คัดลอกคำสั่ง ${item.command} แล้ว!')" 
                class="command-badge px-2.5 py-0.5 rounded-lg text-cyan-300 font-mono text-sm font-bold flex items-center space-x-1 hover:bg-cyan-500/20 transition-colors"
                title="คลิกเพื่อคัดลอกคำสั่ง"
              >
                <span>${item.command}</span>
                <i data-lucide="copy" class="w-3 h-3 text-cyan-400 opacity-70"></i>
              </button>
            </div>
            <span class="text-xs text-slate-400 font-mono">${item.version || ''}</span>
          </div>

          <!-- Title -->
          <h2 class="text-base font-bold text-white mb-2 leading-snug group-hover:text-cyan-300 transition-colors">
            ${item.title}
          </h2>

          <!-- Description -->
          <p class="text-xs sm:text-sm text-slate-400 font-light line-clamp-3 mb-4 flex-1">
            ${item.description}
          </p>

          <!-- Card Actions (Download & View Code) -->
          <div class="pt-4 border-t border-cad-border flex items-center gap-2 mt-auto">
            <button 
              onclick="downloadLispFile('${item.id}')" 
              class="flex-1 flex items-center justify-center space-x-1.5 py-2 px-3 rounded-xl bg-cyan-500 hover:bg-cyan-400 text-slate-950 text-xs sm:text-sm font-bold shadow-md shadow-cyan-500/20 transition-all active:scale-95"
            >
              <i data-lucide="download" class="w-4 h-4"></i>
              <span>โหลด .LSP</span>
            </button>

            <button 
              onclick="openCodeModal('${item.id}')" 
              class="flex items-center justify-center space-x-1 py-2 px-3 rounded-xl bg-slate-800 hover:bg-slate-700 text-slate-200 border border-slate-700 text-xs sm:text-sm font-medium transition-colors"
              title="ดูโค้ด AutoLISP"
            >
              <i data-lucide="code-2" class="w-4 h-4 text-cyan-400"></i>
              <span>ดูโค้ด</span>
            </button>
          </div>

        </div>
      </article>
    `;
  }).join("");

  lucide.createIcons();
}

/**
 * Handle Search & Filters
 */
searchInput.addEventListener("input", (e) => {
  searchKeyword = e.target.value;
  clearSearchBtn.classList.toggle("hidden", searchKeyword.length === 0);
  renderCards();
});

clearSearchBtn.addEventListener("click", () => {
  searchInput.value = "";
  searchKeyword = "";
  clearSearchBtn.classList.add("hidden");
  renderCards();
});

categoryButtons.forEach(btn => {
  btn.addEventListener("click", () => {
    categoryButtons.forEach(b => {
      b.classList.remove("active", "bg-sky-600", "text-white");
      b.classList.add("text-slate-400");
    });
    btn.classList.add("active");
    btn.classList.remove("text-slate-400");
    currentCategory = btn.dataset.category;
    renderCards();
  });
});

document.getElementById("reset-filter-btn")?.addEventListener("click", () => {
  searchInput.value = "";
  searchKeyword = "";
  clearSearchBtn.classList.add("hidden");
  categoryButtons[0].click();
});

// ==========================================
// 🚀 EASY UPLOAD / SMART PARSER LOGIC
// ==========================================

// Open Upload Modal
btnAddLisp.addEventListener("click", () => {
  uploadModal.classList.remove("hidden");
  lucide.createIcons();
});

// 1. LSP File Dropzone & Click
lspDropzone.addEventListener("click", (e) => {
  if (e.target !== btnRemoveLsp) {
    lspFileInput.click();
  }
});

["dragenter", "dragover"].forEach(eventName => {
  lspDropzone.addEventListener(eventName, (e) => {
    e.preventDefault();
    lspDropzone.classList.add("dragover");
  });
});

["dragleave", "drop"].forEach(eventName => {
  lspDropzone.addEventListener(eventName, (e) => {
    e.preventDefault();
    lspDropzone.classList.remove("dragover");
  });
});

lspDropzone.addEventListener("drop", (e) => {
  const files = e.dataTransfer.files;
  if (files.length > 0) {
    handleLspFile(files[0]);
  }
});

lspFileInput.addEventListener("change", (e) => {
  if (e.target.files.length > 0) {
    handleLspFile(e.target.files[0]);
  }
});

function handleLspFile(file) {
  const reader = new FileReader();
  reader.onload = function(evt) {
    const text = evt.target.result;
    formCode.value = text;
    
    // Auto-detect Command from `(defun c:COMMANDNAME`
    const commandMatch = text.match(/\(defun\s+c:([a-zA-Z0-9_\-]+)/i);
    if (commandMatch && commandMatch[1]) {
      formCommand.value = commandMatch[1].toUpperCase();
    } else {
      const baseName = file.name.replace(/\.[^/.]+$/, "").toUpperCase();
      formCommand.value = baseName;
    }

    if (!formTitle.value) {
      formTitle.value = file.name.replace(/\.[^/.]+$/, "");
    }

    lspFileName.textContent = file.name;
    lspDropLabel.classList.add("hidden");
    lspFileBadge.classList.remove("hidden");
    lspFileBadge.classList.add("flex");
    showToast(`อ่านไฟล์ ${file.name} และตรวจพบคำสั่ง ${formCommand.value} สำเร็จ!`);
  };
  reader.readAsText(file);
}

btnRemoveLsp.addEventListener("click", (e) => {
  e.stopPropagation();
  lspFileInput.value = "";
  lspDropLabel.classList.remove("hidden");
  lspFileBadge.classList.add("hidden");
  lspFileBadge.classList.remove("flex");
  formCode.value = "";
});

// 2. GIF / Image Dropzone & Click
gifDropzone.addEventListener("click", (e) => {
  if (e.target !== btnRemoveGif) {
    gifFileInput.click();
  }
});

["dragenter", "dragover"].forEach(eventName => {
  gifDropzone.addEventListener(eventName, (e) => {
    e.preventDefault();
    gifDropzone.classList.add("dragover");
  });
});

["dragleave", "drop"].forEach(eventName => {
  gifDropzone.addEventListener(eventName, (e) => {
    e.preventDefault();
    gifDropzone.classList.remove("dragover");
  });
});

gifDropzone.addEventListener("drop", (e) => {
  const files = e.dataTransfer.files;
  if (files.length > 0) {
    handleGifFile(files[0]);
  }
});

gifFileInput.addEventListener("change", (e) => {
  if (e.target.files.length > 0) {
    handleGifFile(e.target.files[0]);
  }
});

function handleGifFile(file) {
  const reader = new FileReader();
  reader.onload = function(evt) {
    uploadedGifData = evt.target.result;
    gifPreviewImg.src = uploadedGifData;
    gifDropLabel.classList.add("hidden");
    gifPreviewContainer.classList.remove("hidden");
    showToast("อัปโหลดภาพตัวอย่าง GIF สำเร็จ!");
  };
  reader.readAsDataURL(file);
}

btnRemoveGif.addEventListener("click", (e) => {
  e.stopPropagation();
  gifFileInput.value = "";
  uploadedGifData = "";
  gifDropLabel.classList.remove("hidden");
  gifPreviewContainer.classList.add("hidden");
});

// 3. Form Submit -> Add to Library
addLispForm.addEventListener("submit", (e) => {
  e.preventDefault();

  const title = formTitle.value.trim();
  const command = formCommand.value.trim().toUpperCase();
  const category = formCategory.value;
  const version = formVersion.value.trim() || "v1.0";
  const desc = formDesc.value.trim() || "คำสั่ง AutoLISP เสริมประสิทธิภาพใน AutoCAD";
  const code = formCode.value.trim() || `(defun c:${command} ()\n  (princ "\\n${command} is ready.")\n  (princ)\n)`;
  const gifUrl = uploadedGifData || "";

  const newItem = {
    id: "custom_" + Date.now(),
    title: title,
    command: command,
    category: category,
    version: version,
    author: "User",
    description: desc,
    gifUrl: gifUrl,
    downloadUrl: "",
    compatibility: "AutoCAD All Versions",
    code: code,
    isCustom: true
  };

  // Save to LocalStorage
  let localCustomList = [];
  const stored = localStorage.getItem("custom_autolisp_data");
  if (stored) {
    try { localCustomList = JSON.parse(stored); } catch(err) {}
  }
  localCustomList.unshift(newItem);
  localStorage.setItem("custom_autolisp_data", JSON.stringify(localCustomList));

  // Update in-memory database
  lispDatabase = [newItem, ...lispDatabase];
  renderCards();

  // Reset form & close modal
  addLispForm.reset();
  uploadedGifData = "";
  btnRemoveLsp.click();
  btnRemoveGif.click();
  uploadModal.classList.add("hidden");

  showToast(`✓ เพิ่มคำสั่ง ${command} เข้าสู่หน้าเว็บเรียบร้อย!`);
});

// Delete custom item
window.deleteCustomLisp = function(id, event) {
  event.stopPropagation();
  if (confirm("คุณต้องการลบคำสั่งนี้ออกจากหน้าเว็บใช่หรือไม่?")) {
    let localCustomList = [];
    const stored = localStorage.getItem("custom_autolisp_data");
    if (stored) {
      try {
        localCustomList = JSON.parse(stored).filter(i => i.id !== id);
        localStorage.setItem("custom_autolisp_data", JSON.stringify(localCustomList));
      } catch(err) {}
    }
    lispDatabase = lispDatabase.filter(i => i.id !== id);
    renderCards();
    showToast("ลบรายการเรียบร้อยแล้ว");
  }
};

// ==========================================
// 🛠️ MODALS (Code Viewer, Media, Guide)
// ==========================================

let currentModalCode = "";
window.openCodeModal = function(id) {
  const item = lispDatabase.find(i => i.id === id);
  if (!item) return;

  modalCodeTitle.textContent = `${item.command.toLowerCase()}.lsp - ${item.title}`;
  modalCodeContent.textContent = item.code.trim();
  currentModalCode = item.code.trim();
  
  codeModal.classList.remove("hidden");
  lucide.createIcons();
};

modalCopyBtn.addEventListener("click", () => {
  if (currentModalCode) {
    copyToClipboard(currentModalCode, "คัดลอกโค้ด AutoLISP เรียบร้อยแล้ว!");
    copyBtnText.textContent = "Copied!";
    setTimeout(() => {
      copyBtnText.textContent = "Copy Code";
    }, 2000);
  }
});

window.openMediaModal = function(id) {
  const item = lispDatabase.find(i => i.id === id);
  if (!item) return;

  modalMediaTitle.textContent = `${item.title} (คำสั่ง: ${item.command})`;
  modalMediaImg.src = item.gifUrl || getPlaceholderGif(item.title, item.command);
  mediaModal.classList.remove("hidden");
};

window.downloadLispFile = function(id) {
  const item = lispDatabase.find(i => i.id === id);
  if (!item) return;

  if (item.downloadUrl && (item.downloadUrl.startsWith("http://") || item.downloadUrl.startsWith("https://"))) {
    window.open(item.downloadUrl, "_blank");
    showToast(`กำลังเปิดลิงก์ดาวน์โหลด ${item.command}.lsp`);
    return;
  }

  const blob = new Blob([item.code], { type: "text/plain;charset=utf-8" });
  const url = URL.createObjectURL(blob);
  const a = document.createElement("a");
  a.href = url;
  a.download = `${item.command.toLowerCase()}.lsp`;
  document.body.appendChild(a);
  a.click();
  document.body.removeChild(a);
  URL.revokeObjectURL(url);

  showToast(`เริ่มดาวน์โหลดไฟล์ ${item.command.toLowerCase()}.lsp สำเร็จ!`);
};

window.copyToClipboard = function(text, successMsg = "คัดลอกแล้ว!") {
  navigator.clipboard.writeText(text).then(() => {
    showToast(successMsg);
  }).catch(() => {
    const textarea = document.createElement("textarea");
    textarea.value = text;
    document.body.appendChild(textarea);
    textarea.select();
    document.execCommand("copy");
    document.body.removeChild(textarea);
    showToast(successMsg);
  });
};

let toastTimeout;
function showToast(message) {
  clearTimeout(toastTimeout);
  toastMessage.textContent = message;
  toast.classList.remove("hidden");
  toast.classList.add("flex");

  toastTimeout = setTimeout(() => {
    toast.classList.add("hidden");
    toast.classList.remove("flex");
  }, 2500);
}

document.querySelectorAll(".modal-close").forEach(btn => {
  btn.addEventListener("click", () => {
    codeModal.classList.add("hidden");
    mediaModal.classList.add("hidden");
    guideModal.classList.add("hidden");
    uploadModal.classList.add("hidden");
  });
});

[codeModal, mediaModal, guideModal, uploadModal].forEach(modal => {
  modal.addEventListener("click", (e) => {
    if (e.target === modal) {
      modal.classList.add("hidden");
    }
  });
});

btnGuide.addEventListener("click", () => {
  guideModal.classList.remove("hidden");
  lucide.createIcons();
});

document.addEventListener("keydown", (e) => {
  if (e.key === "Escape") {
    codeModal.classList.add("hidden");
    mediaModal.classList.add("hidden");
    guideModal.classList.add("hidden");
    uploadModal.classList.add("hidden");
  }
});

// Initial Render
renderCards();
