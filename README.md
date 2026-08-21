# 🚀 AutoLISP Showcase & Download Hub

เทมเพลตเว็บไซต์สำหรับจัดแสดงผลงาน **AutoLISP** พร้อม **GIF Preview**, **ระบบค้นหาคำสั่ง**, **ตัวดูโค้ด** และ **ปุ่มดาวน์โหลดไฟล์ (.lsp)**

---

## 📂 โครงสร้างโฟลเดอร์

```
autolisp-showcase-hub/
│── index.html          # หน้าเว็บหลัก
│── styles.css          # ไฟล์ตกแต่งสไตล์ (CAD Dark Theme)
│── app.js              # ฐานข้อมูล LISP และระบบการทำงาน
│── README.md           # คู่มือการใช้งานนี้
└── assets/
    ├── gifs/           # โฟลเดอร์สำหรับวางไฟล์ .gif ตัวอย่างการทำงาน
    └── lsp/            # โฟลเดอร์สำหรับวางไฟล์ .lsp สำหรับดาวน์โหลด
```

---

## ⚡ วิธีเปิดใช้งานทันที
ดับเบิลคลิกเปิดไฟล์ `index.html` ใน Google Chrome / Microsoft Edge / Safari ได้ทันทีโดยไม่ต้องลงโปรแกรมเสริมใดๆ

---

## 🛠️ วิธีเพิ่ม / แก้ไขรายการ AutoLISP ของคุณ

เปิดไฟล์ `app.js` ด้วยโปรแกรมแก้ไขข้อความ (เช่น VS Code, Notepad, Notepad++) แล้วแก้ไขในตัวแปร `lispDatabase` ด้านบน:

```javascript
{
  id: "ชื่อไอดีภาษาอังกฤษ",
  title: "ชื่อคำสั่ง / ชื่อโปรแกรมภาษาไทยหรืออังกฤษ",
  command: "คำสั่งย่อที่พิมพ์ใน AutoCAD เช่น NUMSEQ หรือ QLAY",
  category: "text", // เลือกหมวด: "text" | "layer" | "dim" | "draw" | "util"
  version: "v1.0",
  author: "ชื่อของคุณ",
  description: "คำอธิบายการทำงานสั้นๆ เข้าใจง่าย",
  gifUrl: "assets/gifs/ชื่อไฟล์ภาพ.gif", // หรือใส่ URL รูปภาพ/GIF ออนไลน์
  downloadUrl: "assets/lsp/ชื่อไฟล์.lsp", // หรือใส่ลิงก์ Google Drive
  compatibility: "AutoCAD / BricsCAD / ZWCAD",
  code: `;;; วางโค้ด AutoLISP ของคุณที่นี่
(defun c:MYCOMMAND ()
  (princ "\nHello CAD")
  (princ)
)`
}
```

> **💡 ข้อดีของระบบนี้:**
> หากคุณไม่ได้ใส่ไฟล์ `.lsp` ไว้ในโฟลเดอร์ `assets/lsp/` ระบบจะ **สร้างไฟล์ `.lsp` ให้ผู้ใช้ดาวน์โหลดจากข้อความโค้ดในตัวแปร `code` ให้อัตโนมัติทันที!**

---

## 🖼️ คำแนะนำการเตรียมภาพ GIF Preview

1. **การบันทึกหน้าจอเป็น GIF:**
   - แนะนำโปรแกรมฟรี เช่น **ScreenToGif** หรือ **LICEcap** บันทึกเฉพาะหน้าต่าง AutoCAD ขนาดประมาณ `600x350` ถึง `800x450` พิกเซล
   - บันทึกไฟล์ไว้ที่โฟลเดอร์ `assets/gifs/` เช่น `assets/gifs/my-command.gif`
2. **หากยังไม่มีภาพ GIF:**
   - ระบบจะมีภาพจำลอง CAD Crosshair สวยงามขึ้นแทนให้อัตโนมัติ (Fallback SVG Placeholder)

---

## 🌐 วิธีนำขึ้นเว็บไซต์ออนไลน์ฟรี (Free Hosting)

### 1. ใช้ GitHub Pages (แนะนำที่สุด)
1. นำโฟลเดอร์นี้อัปโหลดขึ้น GitHub Repository ของคุณ
2. ไปที่เมนู **Settings** $\rightarrow$ **Pages**
3. ที่หัวข้อ **Branch** เลือก `main` หรือ `master` $\rightarrow$ กด **Save**
4. จะได้ URL เว็บไซต์ เช่น `https://username.github.io/autolisp-hub/` ใช้งานฟรีตลอดไป

### 2. ใช้ Netlify Drop (ลากแล้ววางใช้งานได้ใน 10 วินาที)
1. เข้าเว็บ [app.netlify.com/drop](https://app.netlify.com/drop)
2. ลากโฟลเดอร์ `autolisp-showcase-hub` ไปวาง
3. คุณจะได้ URL เว็บไซต์พร้อมใช้งานทันที

---

## 📋 คำสั่งที่เตรียมไว้เป็นตัวอย่างในระบบ

1. **NUMSEQ (Auto Numbering Sequence):** รันตัวเลขต่อเนื่องอัตโนมัติ
2. **SUMTEXT (Sum Numbers in Text):** รวมยอดตัวเลขจาก Text ในแบบ
3. **QLAY (Quick Layer Match):** สลับและจัดการเลเยอร์อย่างรวดเร็ว
4. **DIMALIGN (Dimension Alignment):** จัดระเบียบแนวเส้นบอกขนาด
5. **CAREA (Total Area Multi-Polyline):** คำนวณผลรวมพื้นที่หลายๆ แปลง
6. **PURGEALL (Deep CAD Cleaner):** ล้างไฟล์ขยะ DGN และ Audit ซ่อมไฟล์ CAD
