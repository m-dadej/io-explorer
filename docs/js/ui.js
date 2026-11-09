// =============================================================================
// Building blocks of the page: controls in the sidebar, value boxes, cards with a
// chart and a table, sortable tables, Excel downloads, number formats.
// =============================================================================

// ---- DOM helper -------------------------------------------------------------------------------
export function el(tag, attrs = {}, ...kids) {
  const node = document.createElement(tag);
  for (const [k, v] of Object.entries(attrs ?? {})) {
    if (v === null || v === undefined || v === false) continue;
    if (k === 'class') node.className = v;
    else if (k === 'html') node.innerHTML = v;
    else if (k.startsWith('on')) node.addEventListener(k.slice(2), v);
    else node.setAttribute(k, v === true ? '' : v);
  }
  for (const kid of kids.flat()) if (kid !== null && kid !== undefined && kid !== false) node.append(kid);
  return node;
}

// ---- Number formats ------------------------------------------------------------------------------
const nf = d => new Intl.NumberFormat('en-GB', { minimumFractionDigits: d, maximumFractionDigits: d });
export const fmt = {
  num: (x, d = 0) => Number.isFinite(x) ? nf(d).format(x) : '–',
  pct: (x, d = 1) => Number.isFinite(x) ? `${nf(d).format(100 * x)}%` : '–',
  bn: (x, d = 1) => Number.isFinite(x) ? `EUR ${nf(d).format(x / 1000)} bn` : '–',
  m: (x, d = 0) => Number.isFinite(x) ? `EUR ${nf(d).format(x)} m` : '–',
  eur: (x, d = 2) => Number.isFinite(x) ? `EUR ${nf(d).format(x)}` : '–',
  // An amount in EUR million: in billions from EUR 1 bn, else in millions
  amount: x => !Number.isFinite(x) ? '–' : Math.abs(x) >= 1000 ? `EUR ${nf(2).format(x / 1000)} bn` : `EUR ${nf(1).format(x)} m`
};
const cellFormat = {
  text: x => x ?? '',
  pct: x => fmt.pct(x, 2),
  eur: x => fmt.num(x, 0),
  eur1: x => fmt.num(x, 1),
  dec: x => fmt.num(x, 2),
  num: x => fmt.num(x, 0),
  // the row says its format (row.type: pct, eur or dec), e.g. a table of key figures
  auto: (x, row) => row.type === 'pct' ? fmt.pct(x, 1) : row.type === 'eur' ? fmt.num(x, 0) : fmt.num(x, 2)
};

// ---- Controls --------------------------------------------------------------------------------------
// spec: list of {key, type, label, value, ...}; types: multi (Tom Select with groups), select,
// checks, radio, number, checkbox, hint, hr, heading, button, group ({when: state => bool, items}).
// Returns the state object (kept up to date); onChange(key) is called after every change.
export function buildControls(container, spec, onChange) {
  const state = {}, groups = [];
  let ready = false;   // no change events while the controls are being built
  const changed = key => { refreshGroups(); if (ready) onChange(key); };
  const refreshGroups = () => groups.forEach(g => { g.node.hidden = !g.when(state); });
  const api = { state, set: {} };

  function field(label, ...kids) {
    return el('div', { class: 'field' }, label ? el('div', { class: 'field-label' }, label) : null, ...kids);
  }
  function add(parent, item) {
    switch (item.type) {
      case 'group': {
        const node = el('div', { class: 'group' });
        item.items.forEach(it => add(node, it));
        groups.push({ node, when: item.when });
        parent.append(node);
        break;
      }
      case 'multi': {
        const select = el('select', { multiple: true, 'aria-label': item.label });
        parent.append(field(item.label, select));
        state[item.key] = [...item.value];
        const ts = new TomSelect(select, {
          options: item.options.flatMap(g => g.items.map(o => ({ ...o, optgroup: g.label }))),
          optgroups: item.options.map(g => ({ value: g.label, label: g.label })),
          optgroupField: 'optgroup', valueField: 'value', labelField: 'text', searchField: ['text', 'value'],
          items: item.value, plugins: ['remove_button'], maxOptions: 500, hidePlaceholder: true, dropdownParent: 'body',
          placeholder: 'Type to search…', onChange: v => { state[item.key] = v; changed(item.key); },
          onItemAdd() { this.setTextboxValue(''); this.refreshOptions(false); }
        });
        api.set[item.key] = v => { ts.setValue(v, true); state[item.key] = [...v]; };
        break;
      }
      case 'select': {
        const select = el('select', { onchange: e => { state[item.key] = e.target.value; changed(item.key); } },
          item.choices.map(([v, t]) => el('option', { value: v, selected: v === item.value }, t)));
        state[item.key] = item.value;
        parent.append(field(item.label, select));
        api.set[item.key] = v => { select.value = v; state[item.key] = v; };
        break;
      }
      case 'checks': {
        state[item.key] = [...item.value];
        const boxes = item.choices.map(([v, t]) => el('label', { class: 'check' },
          el('input', { type: 'checkbox', value: v, checked: item.value.includes(v),
            onchange: () => { state[item.key] = boxes.map(b => b.querySelector('input')).filter(i => i.checked).map(i => i.value); changed(item.key); } }),
          ' ', t));
        parent.append(field(item.label, el('div', { class: 'checks' }, boxes)));
        api.set[item.key] = v => { boxes.forEach(b => { const i = b.querySelector('input'); i.checked = v.includes(i.value); }); state[item.key] = [...v]; };
        break;
      }
      case 'radio': {
        const name = `r${Math.random().toString(36).slice(2)}`;
        state[item.key] = item.value;
        const opts = item.choices.map(([v, t]) => el('label', { class: 'check' },
          el('input', { type: 'radio', name, value: v, checked: v === item.value,
            onchange: e => { state[item.key] = e.target.value; changed(item.key); } }), ' ', t));
        parent.append(field(item.label, el('div', { class: item.inline ? 'radios inline' : 'radios' }, opts)));
        api.set[item.key] = v => { opts.forEach(o => { const i = o.querySelector('input'); i.checked = i.value === v; }); state[item.key] = v; };
        break;
      }
      case 'number': {
        const input = el('input', { type: 'number', value: item.value, min: item.min, max: item.max, step: item.step ?? 'any',
          oninput: e => { state[item.key] = e.target.value === '' ? null : Number(e.target.value); changed(item.key); } });
        state[item.key] = item.value;
        parent.append(field(item.label, input));
        api.set[item.key] = v => { input.value = v; state[item.key] = v; };
        break;
      }
      case 'checkbox': {
        const input = el('input', { type: 'checkbox', checked: item.value,
          onchange: e => { state[item.key] = e.target.checked; changed(item.key); } });
        state[item.key] = item.value;
        parent.append(el('div', { class: 'field' }, el('label', { class: 'check' }, input, ' ', item.label)));
        api.set[item.key] = v => { input.checked = v; state[item.key] = v; };
        break;
      }
      case 'button':
        parent.append(el('button', { class: item.primary ? 'btn btn-primary' : 'btn', onclick: item.onClick }, item.label));
        break;
      case 'hint': parent.append(el('div', { class: 'hint', html: item.html })); break;
      case 'heading': parent.append(el('div', { class: 'side-heading' }, item.label)); break;
      case 'hr': parent.append(el('hr')); break;
    }
  }
  spec.forEach(it => add(container, it));
  refreshGroups();
  ready = true;
  api.refresh = refreshGroups;
  return api;
}

// ---- Value boxes ------------------------------------------------------------------------------------
export function valueBoxes(container, items) {
  const nodes = {};
  const wrap = el('div', { class: 'boxes' }, items.map(it => {
    const t = el('div', { class: 't' }, it.title), v = el('div', { class: 'v' }, '…'), n = el('div', { class: 'n' }, it.note ?? '');
    nodes[it.key] = { t, v, n };
    return el('div', { class: 'box' }, t, v, n);
  }));
  container.append(wrap);
  return {
    set(key, value, note, title) {
      nodes[key].v.textContent = value;
      if (note !== undefined) nodes[key].n.textContent = note;
      if (title !== undefined) nodes[key].t.textContent = title;
    }
  };
}

// ---- Cards: chart + table + download ------------------------------------------------------------------
// The chart is drawn by a function of the available width; it is drawn again when the card resizes.
// tableOnly: a card with just the table (and its note and download); pageSize: rows per page of the table.
export function card(container, { title, top = null, note = null, tableOnly = false, pageSize = 15 }) {
  const titleEl = el('div', { class: 'card-title' }, title);
  const btnChart = el('button', { class: 'active', onclick: () => show('chart') }, 'Chart');
  const btnTable = el('button', { onclick: () => show('table') }, 'Table');
  const chartEl = el('div', { class: 'chart' });
  const partsEl = el('div', { class: 'parts', hidden: true });
  const tableEl = el('div', { class: 'table-wrap' });
  const dlBtn = el('button', { class: 'btn btn-small', onclick: () => download() }, 'Download (Excel)');
  const noteEl = () => note ? el('div', { class: 'note' }, note) : null;
  const chartPane = el('div', { class: 'pane', hidden: tableOnly }, top, partsEl, chartEl, tableOnly ? null : noteEl());
  const tablePane = el('div', { class: 'pane', hidden: !tableOnly }, tableEl, tableOnly ? noteEl() : null, el('div', { class: 'dl' }, dlBtn));
  const node = el('div', { class: 'card' },
    el('div', { class: 'card-head' }, titleEl, tableOnly ? null : el('div', { class: 'card-tabs' }, btnChart, btnTable)),
    el('div', { class: 'card-body' }, chartPane, tablePane));
  container.append(node);

  let draw = null, rows = [], cols = [], stub = 'data', lastWidth = 0;
  function show(which) {
    chartPane.hidden = which !== 'chart'; tablePane.hidden = which !== 'table';
    btnChart.classList.toggle('active', which === 'chart'); btnTable.classList.toggle('active', which === 'table');
    if (which === 'chart') redraw(true);
  }
  function redraw(force = false) {
    const w = chartEl.clientWidth;
    if (!draw || w === 0 || (!force && Math.abs(w - lastWidth) < 4)) return;
    lastWidth = w;
    try { chartEl.replaceChildren(draw(w)); } catch (e) { chartEl.replaceChildren(message(e.message, true)); }
  }
  new ResizeObserver(() => redraw()).observe(chartEl);
  async function download() {
    await downloadXlsx([{ name: stub, rows, cols }], `${stub}.xlsx`);
  }
  return {
    node,
    setTitle(t) { titleEl.textContent = t; },
    setParts(text) { partsEl.textContent = text ?? ''; partsEl.hidden = !text; },
    // chartFn(width) -> node; data rows; cols [{key, label, type}]
    set({ chart, data, columns, file }) {
      draw = chart ?? null; rows = data; cols = columns; stub = file ?? stub;
      if (draw) redraw(true); else chartEl.replaceChildren();
      renderTable(tableEl, rows, cols, pageSize);
      dlBtn.disabled = false;
    },
    message(text, isError = false) {
      draw = null; rows = []; this.setParts(null);
      chartEl.replaceChildren(message(text, isError));
      tableEl.replaceChildren(message(text, isError));
      dlBtn.disabled = true;
    }
  };
}
export function message(text, isError = false) { return el('div', { class: isError ? 'msg err' : 'msg' }, text); }

// ---- Tables: sortable, searchable, paged ----------------------------------------------------------------
export function renderTable(container, rows, cols, pageSize = 15) {
  let sortKey = null, sortDir = 1, page = 0, size = pageSize, query = '';
  const search = el('input', { type: 'search', placeholder: 'Search…', class: 'table-search',
    oninput: e => { query = e.target.value.toLowerCase(); page = 0; draw(); } });
  const tbl = el('table', { class: 'data' });
  const pager = el('div', { class: 'pager' });
  container.replaceChildren(search, el('div', { class: 'table-scroll' }, tbl), pager);
  function draw() {
    const textCols = cols.filter(c => c.type === 'text');
    let r = query ? rows.filter(row => textCols.some(c => String(row[c.key] ?? '').toLowerCase().includes(query))) : rows;
    if (sortKey) {
      const c = cols.find(x => x.key === sortKey);
      r = [...r].sort((a, b) => {
        const x = a[sortKey], y = b[sortKey];
        if (c.type === 'text') return sortDir * String(x).localeCompare(String(y));
        return sortDir * ((Number.isFinite(x) ? x : -Infinity) - (Number.isFinite(y) ? y : -Infinity));
      });
    }
    const pages = Math.max(1, Math.ceil(r.length / size));
    page = Math.min(page, pages - 1);
    const shown = r.slice(page * size, (page + 1) * size);
    tbl.replaceChildren(
      el('thead', {}, el('tr', {}, cols.map(c => el('th', {
        class: c.type === 'text' ? 'txt' : '', title: 'Sort',
        onclick: () => { if (sortKey === c.key) sortDir = -sortDir; else { sortKey = c.key; sortDir = c.type === 'text' ? 1 : -1; } draw(); }
      }, c.label, sortKey === c.key ? (sortDir > 0 ? ' ▲' : ' ▼') : '')))),
      el('tbody', {}, shown.map(row => el('tr', {}, cols.map(c => el('td', { class: c.type === 'text' ? 'txt' : '' }, cellFormat[c.type](row[c.key], row)))))));
    pager.replaceChildren(
      el('span', {}, `${r.length} rows`),
      el('button', { class: 'btn btn-small', disabled: page === 0, onclick: () => { page--; draw(); } }, '‹'),
      el('span', {}, `page ${page + 1} of ${pages}`),
      el('button', { class: 'btn btn-small', disabled: page >= pages - 1, onclick: () => { page++; draw(); } }, '›'),
      el('select', { onchange: e => { size = Number(e.target.value); page = 0; draw(); } },
        [15, 50, 200, 5000].map(s => el('option', { value: s, selected: s === size }, s === 5000 ? 'all' : `${s} per page`))));
  }
  draw();
}

// ---- Excel downloads (the library is loaded only when needed) -----------------------------------------------
let xlsxLoading = null;
function loadScript(src) {
  return new Promise((resolve, reject) => {
    const s = el('script', { src });
    s.onload = resolve; s.onerror = () => reject(new Error(`Could not load ${src}`));
    document.head.append(s);
  });
}
export async function downloadXlsx(sheets, filename) {
  if (!window.XLSX) { xlsxLoading ??= loadScript('lib/xlsx.full.min.js'); await xlsxLoading; }
  const wb = XLSX.utils.book_new();
  for (const s of sheets) {
    const aoa = s.cols
      ? [s.cols.map(c => c.label), ...s.rows.map(r => s.cols.map(c => { const v = r[c.key]; return typeof v === 'number' && !Number.isFinite(v) ? null : v; }))]
      : s.aoa;
    XLSX.utils.book_append_sheet(wb, XLSX.utils.aoa_to_sheet(aoa), s.name.slice(0, 31));
  }
  XLSX.writeFile(wb, filename);
}

// ---- Run a function at most every `ms` milliseconds after the last call ---------------------------------------
export function debounce(fn, ms = 150) {
  let t = null;
  return (...args) => { clearTimeout(t); t = setTimeout(() => fn(...args), ms); };
}
