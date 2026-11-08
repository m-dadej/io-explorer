// =============================================================================
// IO Explorer in the browser: loads one year of FIGARO data and builds the tabs.
// The model itself is in engine.js; charts in charts.js; page parts in ui.js.
// Data loads in three steps: core tables at once (8 MB), the Leontief inverse (41 MB)
// when a tab needs it, the input coefficients (38 MB) for shocks.
// =============================================================================

import { IOModel, parseCore, parseL, parseA, sum } from './engine.js';
import { el, fmt, buildControls, valueBoxes, card, message, downloadXlsx, debounce } from './ui.js';
import { barsH, stackedH, pairedH, heatmap, categoryColours, effectColours, contentColours, multiplierColours,
         hhGreen, hhLabel, blue } from './charts.js';

let model, meta, D, year, base;
const mainEl = document.getElementById('main');

// ---- Errors are shown on the page (and not only in the console) ---------------------------------------
const errorsEl = document.getElementById('errors');
function showError(msg) { errorsEl.hidden = false; errorsEl.append(el('div', {}, msg)); }
window.addEventListener('error', e => showError(`Error: ${e.message}`));
window.addEventListener('unhandledrejection', e => showError(`Error: ${e.reason?.message ?? e.reason}`));

// ---- Loading with a progress bar -------------------------------------------------------------------------
const statusEl = document.getElementById('status');
const progress = new Map();
function drawStatus() {
  statusEl.hidden = progress.size === 0;
  statusEl.replaceChildren(...[...progress].map(([label, [got, total]]) => el('div', { class: 'status-row' },
    el('div', {}, `Loading ${label}… ${fmt.num(got / 1e6, 0)} of ${fmt.num(total / 1e6, 0)} MB`),
    el('div', { class: 'bar' }, el('span', { style: `width:${total ? Math.min(100, 100 * got / total) : 0}%` })))));
}
async function fetchBuffer(url, label, total) {
  const res = await fetch(url);
  if (!res.ok) throw new Error(`Could not load ${url} (${res.status})`);
  if (!res.body) return res.arrayBuffer();
  const reader = res.body.getReader(), chunks = [];
  let got = 0;
  progress.set(label, [0, total]); drawStatus();
  for (;;) {
    const { done, value } = await reader.read();
    if (done) break;
    chunks.push(value); got += value.length;
    progress.set(label, [got, total]); drawStatus();
  }
  progress.delete(label); drawStatus();
  const out = new Uint8Array(got);
  let off = 0;
  for (const c of chunks) { out.set(c, off); off += c.length; }
  return out.buffer;
}
const loads = {};
function ensure(...parts) {
  return Promise.all(parts.map(p => {
    loads[p] ??= (async () => {
      const label = p === 'L' ? 'the Leontief inverse' : 'the input coefficients';
      const buf = await fetchBuffer(`${base}/${meta.files[p]}`, label, meta.bytes[p]);
      if (p === 'L') model.L = parseL(buf); else model.A = parseA(meta, buf);
    })().catch(e => { delete loads[p]; throw e; });
    return loads[p];
  }));
}

// ---- Shared bits ------------------------------------------------------------------------------------------
let countryOpts, sectorOpts, catChoices;
function pickerOptions() {
  countryOpts = [
    { label: 'Groups', items: Object.keys(meta.country_groups).map(g => ({ value: g, text: g })) },
    { label: 'Countries', items: meta.countries.map(c => ({ value: c.code, text: `${c.name} (${c.code})` })) }
  ];
  sectorOpts = [
    { label: 'All', items: [{ value: 'All sectors', text: 'All sectors' }] },
    { label: 'Broad groups', items: meta.broad_groups.map(g => ({ value: g, text: g })) },
    { label: 'Your groups', items: meta.custom_groups.map(g => ({ value: g, text: g })) },
    { label: 'Industries', items: meta.sectors.map(s => ({ value: s.code, text: `${s.code}  ${s.name}` })) }
  ];
  catChoices = meta.fd_categories.map(k => [k.code, k.label]);
}
function needSel(...lists) {
  if (lists.some(l => !l || l.length === 0)) throw new Error('Make a choice on the left.');
}
const topOf = (x, d) => Number.isFinite(x) && x >= 1 ? Math.round(x) : d;
const minVaOf = x => Number.isFinite(x) ? x : 0;
function inlineRadio(choices, value, onChange) {
  const name = `r${Math.random().toString(36).slice(2)}`;
  return el('div', { class: 'radios inline card-switch' }, choices.map(([v, t]) => el('label', { class: 'check' },
    el('input', { type: 'radio', name, value: v, checked: v === value, onchange: e => onChange(e.target.value) }), ' ', t)));
}
function checkSpendRate(r) {
  if (!(Number.isFinite(r) && r >= 0 && r <= 1)) throw new Error('Consumer spending per EUR of extra wages must be between 0 and 1.');
  return r;
}
function checkSize(size) {
  if (!Number.isFinite(size) || size === 0) throw new Error('Give a size other than 0.');
  return size;
}
const induceText = rate => `With household spending of extra wages (EUR ${rate.toFixed(2)} per EUR).`;
const describeSectors = s => s.length === 1 && s[0] === 'All sectors' ? 'all industries' : s.map(x => model.sectorNames[x] ?? x).join(' + ');

// Long rows for a stacked chart of the parts of an effect; d has direct, first, further, induced (EUR m)
function effectRows(items, labelOf, pct) {
  const parts = [['direct', 'Direct'], ['first', 'First-round suppliers'], ['further', 'Further supply-chain rounds'],
                 ['induced', 'Household spending']];
  const rows = items.flatMap(d => parts.map(([k, name]) => ({ label: labelOf(d), category: name, value: (d[k] || 0) / (pct ? d.va : 1) })));
  const used = new Set(rows.filter(r => r.value !== 0).map(r => r.category));
  return rows.filter(r => used.has(r.category));
}
function partsText(where, total, direct, first, further, induced, production, showInduced) {
  const parts = [
    !production ? `direct ${fmt.m(direct, 1)}` : (direct !== 0 ? `the shocked producers themselves ${fmt.m(direct, 1)} (not in the chart)` : null),
    `first-round suppliers ${fmt.m(first, 1)}`, `further supply-chain rounds ${fmt.m(further, 1)}`,
    showInduced ? `household spending ${fmt.m(induced, 1)}` : null].filter(Boolean);
  return `Change in GVA ${where}: ${fmt.m(total, 1)} = ${parts.join(', ')}.`;
}
const COLS = {
  country: { key: 'country_name', label: 'Country', type: 'text' },
  va: { key: 'va', label: 'GVA, EUR m', type: 'eur' }
};
const nameCol = level => ({ country: 'Country', country_sector: 'Country-industry', sector: 'Industry', sector_group: 'Industry group' })[level];

function page(id, withSidebar = true) {
  const side = withSidebar ? el('aside', { class: 'sidebar' }) : null;
  const content = el('div', { class: 'content' });
  const sec = el('section', { class: withSidebar ? 'page' : 'page single', id: `page-${id}`, hidden: true }, side, content);
  mainEl.append(sec);
  return { sec, side, content };
}

// ==== 1. Where value added ends up =============================================================================
function buildDest() {
  const { side, content } = page('dest');
  const ctl = buildControls(side, [
    { key: 'c', type: 'multi', label: 'Origin: countries', value: [D.origin], options: countryOpts },
    { key: 's', type: 'multi', label: 'Origin: industries', value: ['All sectors'], options: sectorOpts },
    { key: 'cats', type: 'checks', label: 'Final demand categories', value: catChoices.map(c => c[0]), choices: catChoices },
    { key: 'top', type: 'number', label: 'Rows in the chart', value: 15, min: 5, max: 60, step: 5 },
    { type: 'hint', html: 'Value added (GVA) of the origin that ends up in final demand of each buyer country, directly or through other countries’ exports (e.g. Polish parts in German cars bought in the US count for the US). Shares are of the origin’s GVA.' }
  ], () => update());
  const S = ctl.state;
  const boxes = valueBoxes(content, [
    { key: 'gva', title: 'Gross value added' },
    { key: 'home', title: 'Ends up at home', note: 'share of GVA, all categories' },
    { key: 'abroad', title: 'Ends up abroad', note: 'share of GVA, all categories' },
    { key: 'gov', title: 'Depends on government consumption', note: 'at home and abroad' }]);
  const cTop = card(content, { title: 'Foreign buyers: share of the origin’s GVA absorbed by their final demand' });
  const cExp = card(content, { title: 'Direct exports vs value added absorbed (shares of all foreign buyers)',
    note: 'Gross exports count where goods and services are shipped to. Value added absorbed counts where the origin’s own value added is finally used. A buyer with a longer blue bar takes more of the origin’s value added than direct exports suggest (it is reached through other countries).' });
  let heatRows = 'sector';
  const cHeat = card(content, { title: 'Share of GVA absorbed, by part of the origin (%)',
    top: inlineRadio([['sector', 'By industry'], ['country', 'By country']], 'sector', v => { heatRows = v; update(); }),
    note: 'Home = final demand in the row’s own country (not coloured, so that differences between foreign buyers show). Largest 15 rows by GVA, top 8 foreign buyers.' });

  const update = debounce(() => {
    try {
      needSel(S.c, S.s);
      if (!S.cats.length) throw new Error('Choose at least one final demand category.');
      const cats = catChoices.map(c => c[0]).filter(k => S.cats.includes(k));
      const top = topOf(S.top, 15), what = `${model.describe(S.c, S.s)}, ${year}.`;
      const all = model.vaByBuyer(S.c, S.s, 'total');
      const ova = all[0].origin_va;
      const share = f => sum(all.filter(f).map(r => r.va)) / ova;
      boxes.set('gva', fmt.bn(ova));
      boxes.set('home', fmt.pct(share(r => r.domestic)));
      boxes.set('abroad', fmt.pct(share(r => !r.domestic)));
      boxes.set('gov', fmt.pct(share(r => r.code === 'P3_S13')));

      const byBuyer = new Map();
      for (const r of all) {
        let b = byBuyer.get(r.buyer);
        if (!b) { b = { buyer: r.buyer, buyer_name: r.buyer_name + (r.domestic ? ' (home)' : ''), domestic: r.domestic, total: 0, va: 0 }; byBuyer.set(r.buyer, b); }
        if (cats.includes(r.code)) { b[r.code] = r.share; b.total += r.share; b.va += r.va; }
      }
      const buyers = [...byBuyer.values()].sort((a, b) => (b.domestic - a.domestic) || (b.total - a.total));
      const foreign = buyers.filter(b => !b.domestic);
      const shown = foreign.slice(0, top);
      const home = sum(buyers.filter(b => b.domestic).map(b => b.total));
      const rows = shown.flatMap(b => cats.map(k => ({ label: b.buyer_name, category: model.catLabels[k], value: b[k] ?? 0 })));
      cTop.set({
        chart: w => stackedH(w, { rows, labels: shown.map(b => b.buyer_name), colours: cats.map(k => [model.catLabels[k], categoryColours[k]]),
                                 xTitle: 'Share of the origin’s GVA', caption: `${what} Not shown: home final demand, ${fmt.pct(home)} of GVA.` }),
        data: buyers, file: 'va_by_buyer',
        columns: [{ key: 'buyer_name', label: 'Buyer', type: 'text' }, ...cats.map(k => ({ key: k, label: model.catLabels[k], type: 'pct' })),
                  { key: 'total', label: 'Total share', type: 'pct' }, { key: 'va', label: 'GVA absorbed, EUR m', type: 'eur' }]
      });

      const ex = model.exportsVsVa(S.c, S.s), exTop = ex.slice(0, top);
      cExp.set({
        chart: w => pairedH(w, {
          rows: exTop.flatMap(r => [{ label: r.buyer_name, measure: 'Gross exports', value: r.exports_share },
                                    { label: r.buyer_name, measure: 'Value added absorbed', value: r.va_share }]),
          measures: ['Gross exports', 'Value added absorbed'], labels: exTop.map(r => r.buyer_name),
          xTitle: 'Share of all foreign buyers', caption: `${what} All final demand categories.` }),
        data: ex, file: 'exports_vs_va',
        columns: [{ key: 'buyer_name', label: 'Buyer', type: 'text' }, { key: 'gross_exports', label: 'Gross exports, EUR m', type: 'eur' },
                  { key: 'exports_share', label: 'Share of gross exports', type: 'pct' }, { key: 'va_absorbed', label: 'GVA absorbed, EUR m', type: 'eur' },
                  { key: 'va_share', label: 'Share of GVA absorbed abroad', type: 'pct' }]
      });

      const b = model.vaByBuyer(S.c, S.s, heatRows).filter(r => cats.includes(r.code));
      const origins = [...new Map(b.map(r => [r.origin, r.origin_va]))].sort((x, y) => y[1] - x[1]).slice(0, 15).map(x => x[0]);
      const cols = ['Home', ...foreign.slice(0, 8).map(f => model.shortCode(f.buyer))];
      const nameOf = o => heatRows === 'sector' ? model.sectorNames[o] : model.countryNames[o];
      const cell = new Map();
      for (const r of b) {
        if (!origins.includes(r.origin)) continue;
        const col = r.domestic ? 'Home' : model.shortCode(r.buyer);
        if (!cols.includes(col)) continue;
        const key = `${r.origin}|${col}`;
        cell.set(key, (cell.get(key) ?? 0) + r.share);
      }
      const long = origins.flatMap(o => cols.map(c => ({ row: nameOf(o), col: c, value: cell.get(`${o}|${c}`) ?? 0 })));
      const wide = origins.map(o => Object.fromEntries([['row', nameOf(o)], ...cols.map(c => [c, cell.get(`${o}|${c}`) ?? 0])]));
      cHeat.set({
        chart: w => heatmap(w, { rows: long, rowLevels: origins.map(nameOf), colLevels: cols, plainCols: ['Home'],
                                 caption: `${what} Values in % of the row’s GVA. RoW = rest of world.` }),
        data: wide, file: 'va_by_buyer_heatmap',
        columns: [{ key: 'row', label: 'Origin', type: 'text' }, ...cols.map(c => ({ key: c, label: c, type: 'pct' }))]
      });
    } catch (e) { [cTop, cExp, cHeat].forEach(c => c.message(e.message, true)); }
  }, 120);
  return { update };
}

// ==== 2. Exposure to a source ===================================================================================
function buildExpo() {
  const { side, content } = page('expo');
  const ctl = buildControls(side, [
    { key: 'type', type: 'radio', label: 'The source is', value: 'production',
      choices: [['production', 'Production of a customer industry'], ['final_demand', 'Final demand for products']] },
    { key: 'src_c', type: 'multi', label: 'Source: countries', value: ['DE'], options: countryOpts },
    { key: 'src_s', type: 'multi', label: 'Source: industries', value: ['C29'], options: sectorOpts },
    { type: 'hr' },
    { key: 'show', type: 'multi', label: 'Show results for', value: [D.focus], options: countryOpts },
    { key: 'level', type: 'radio', label: 'Results by', value: 'country_sector',
      choices: [['country_sector', 'Country-industry'], ['country', 'Country'], ['sector', 'Industry'], ['sector_group', 'Broad industry group']] },
    { key: 'min_va', type: 'number', label: 'Leave out country-industries with GVA below (EUR m)', value: D.min_va, min: 0, step: 50 },
    { key: 'top', type: 'number', label: 'Rows in the chart', value: D.top_n, min: 5, max: 60, step: 5 },
    { type: 'hint', html: '<b>Production:</b> share of GVA that would disappear if the source stopped producing (its purchases of inputs, direct and through the supply chain). A 10% fall in the source’s output takes away 10% of this.<br><b>Final demand:</b> share of GVA that depends on final demand, anywhere, for the source’s products.' }
  ], () => update());
  const S = ctl.state;
  const boxes = valueBoxes(content, [
    { key: 'size', title: 'Output of the source' },
    { key: 'dep', title: 'GVA depending on it in the countries shown' },
    { key: 'share', title: 'Share of their GVA' }]);
  const cRes = card(content, { title: 'Share of GVA that depends on the source' });

  const update = debounce(async () => {
    try {
      needSel(S.src_c, S.src_s, S.show);
      const prod = S.type === 'production';
      if (!model.L) cRes.message('Loading the model data…');
      await ensure('L');
      if (prod && model.needsA(model.selectCS(S.src_c, S.src_s))) await ensure('A');
      const imp = model.dependenceImpact(S.type, S.src_c, S.src_s);
      const res = model.dependenceTable(imp, S.type, S.level, S.show, minVaOf(S.min_va));
      const t = model.summariseImpact(imp, 'total', S.show)[0];
      const dep = prod ? t.dva_excl_source : t.dva;
      boxes.set('size', fmt.bn(Math.abs(imp.size)), undefined, prod ? 'Output of the source' : 'Final demand for the source’s products');
      boxes.set('dep', fmt.bn(dep));
      boxes.set('share', fmt.pct(dep / t.va, 2));
      cRes.setTitle(`Share of GVA that depends on ${prod ? 'the production of ' : 'final demand for products of '}${model.describe(S.src_c, S.src_s)}`);
      const top = res.slice(0, topOf(S.top, D.top_n));
      cRes.set({
        chart: w => barsH(w, { rows: top, label: d => d.name, value: d => d.share, xTitle: 'Share of GVA',
                               caption: `${year}. Results for: ${model.describe(S.show)}.`,
                               tip: d => `${d.name}\n${fmt.pct(d.share, 2)} of GVA\n${fmt.m(d.dep, 1)} dependent` }),
        data: res, file: 'exposure_to_source',
        columns: [{ key: 'name', label: nameCol(S.level), type: 'text' }, { key: 'share', label: 'Share of GVA dependent', type: 'pct' },
                  { key: 'dep', label: 'GVA dependent, EUR m', type: 'eur1' }, COLS.va]
      });
    } catch (e) { cRes.message(e.message, true); }
  }, 150);
  return { update };
}

// ==== 3. Sources of an origin =====================================================================================
function buildSrc() {
  const { side, content } = page('src');
  const ctl = buildControls(side, [
    { key: 'c', type: 'multi', label: 'Origin: countries', value: [D.origin], options: countryOpts },
    { key: 's', type: 'multi', label: 'Origin: industries', value: ['All sectors'], options: sectorOpts },
    { key: 'type', type: 'radio', label: 'Depends on', value: 'production',
      choices: [['production', 'Production of customer industries'], ['final_demand', 'Final demand for products']] },
    { key: 'level', type: 'radio', label: 'Sources', value: 'country_sector',
      choices: [['country_sector', 'Single country-industries'], ['country', 'Whole countries'],
                ['sector', 'Industries, all countries together (final demand only)']] },
    { key: 'src_c', type: 'multi', label: 'Look for sources in', value: ['World'], options: countryOpts },
    { key: 'foreign', type: 'checkbox', label: 'Only sources outside the origin’s countries', value: true },
    { key: 'top', type: 'number', label: 'Rows in the chart', value: D.top_n, min: 5, max: 60, step: 5 },
    { type: 'hint', html: '<b>Production:</b> the shares are not additive (supply chains overlap). <b>Final demand:</b> the shares add up to 100% over all sources, home included.' }
  ], () => update());
  const S = ctl.state;
  const cRes = card(content, { title: 'What the origin depends on' });
  const update = debounce(async () => {
    try {
      needSel(S.c, S.s, S.src_c);
      if (S.type === 'production' && S.level === 'sector') throw new Error('For production, choose single country-industries or whole countries as sources.');
      if (!model.L) cRes.message('Loading the model data…');
      await ensure('L');
      const res = model.sourcesOf({ originCountries: S.c, originSectors: S.s, type: S.type, sourceLevel: S.level,
                                    sourceCountries: S.src_c, foreignOnly: S.foreign });
      cRes.setTitle(`What ${model.describe(S.c, S.s)} depends on: ${S.type === 'production' ? 'production of its customers' : 'final demand for products'}`);
      if (!res.length) throw new Error('No sources for this choice.');
      const top = res.slice(0, topOf(S.top, D.top_n));
      cRes.set({
        chart: w => barsH(w, { rows: top, label: d => d.source_name, value: d => d.share, xTitle: 'Share of the origin’s GVA',
                               caption: `${year}. Origin GVA: ${fmt.bn(res[0].origin_va)}.`,
                               tip: d => `${d.source_name}\n${fmt.pct(d.share, 2)} of the origin’s GVA\n${fmt.m(d.va_dependent, 1)}` }),
        data: res, file: 'sources_of_origin',
        columns: [{ key: 'source_name', label: 'Source', type: 'text' }, { key: 'share', label: 'Share of origin’s GVA', type: 'pct' },
                  { key: 'va_dependent', label: 'GVA dependent, EUR m', type: 'eur1' }]
      });
    } catch (e) { cRes.message(e.message, true); }
  }, 150);
  return { update };
}

// ==== 4. Top pairs ===================================================================================================
function buildPairs() {
  const { side, content } = page('pairs');
  const ctl = buildControls(side, [
    { key: 'oc', type: 'multi', label: 'Origins: countries', value: [D.focus], options: countryOpts },
    { key: 'os', type: 'multi', label: 'Origins: industries', value: ['All sectors'], options: sectorOpts },
    { key: 'sc', type: 'multi', label: 'Sources: countries', value: ['World'], options: countryOpts },
    { key: 'ss', type: 'multi', label: 'Sources: industries', value: ['All sectors'], options: sectorOpts },
    { key: 'type', type: 'radio', label: 'Dependence on', value: 'production',
      choices: [['production', 'Production of the source'], ['final_demand', 'Final demand for the source’s products']] },
    { key: 'foreign', type: 'checkbox', label: 'Only sources in another country', value: true },
    { key: 'min_va', type: 'number', label: 'Leave out country-industries with GVA below (EUR m)', value: D.min_va, min: 0, step: 50 },
    { key: 'n', type: 'number', label: 'Pairs in the table', value: 100, min: 10, max: 1000, step: 10 },
    { type: 'hint', html: 'Each pair: one origin country-industry and one source country-industry. Share = part of the origin’s GVA that depends on the source.' }
  ], () => update());
  const S = ctl.state;
  const cRes = card(content, { title: 'Most exposed pairs of country-industries' });
  const update = debounce(async () => {
    try {
      needSel(S.oc, S.os, S.sc, S.ss);
      if (!model.L) cRes.message('Loading the model data…');
      await ensure('L');
      const n = Math.min(1000, topOf(S.n, 100));
      const res = model.topPairs({ originCountries: S.oc, originSectors: S.os, sourceCountries: S.sc, sourceSectors: S.ss,
                                   type: S.type, foreignOnly: S.foreign, minVA: minVaOf(S.min_va), nTop: n });
      if (!res.length) throw new Error('No pairs for this choice.');
      res.forEach(r => { r.pair = `${r.origin} ← ${r.source}`; });
      const top = res.slice(0, 25);
      cRes.set({
        chart: w => barsH(w, { rows: top, label: d => d.pair, value: d => d.share, xTitle: 'Share of the origin’s GVA',
                               caption: `${year}. Origin ← source. Top 25; the table has more.`,
                               tip: d => `${d.origin}\ndepends on ${d.source}\n${fmt.pct(d.share, 2)} of its GVA (${fmt.m(d.va_dependent, 1)})` }),
        data: res, file: 'top_pairs',
        columns: [{ key: 'origin', label: 'Origin', type: 'text' }, { key: 'source', label: 'Source', type: 'text' },
                  { key: 'share', label: 'Share of origin’s GVA', type: 'pct' }, { key: 'va_dependent', label: 'GVA dependent, EUR m', type: 'eur1' },
                  { key: 'origin_va', label: 'Origin GVA, EUR m', type: 'eur' }]
      });
    } catch (e) { cRes.message(e.message, true); }
  }, 150);
  return { update };
}

// ==== Government consumption =========================================================================================
let simRequest = null;   // a scenario sent from the Government tab to the shock simulator
function buildGov() {
  const { side, content } = page('gov');
  const ctl = buildControls(side, [
    { key: 'show', type: 'multi', label: 'Countries', value: [D.focus], options: countryOpts },
    { type: 'hr' },
    { key: 'gov', type: 'select', label: 'Government of', value: 'DE', choices: meta.countries.map(c => [c.code, c.name]) },
    { key: 'level', type: 'radio', label: 'Who benefits, by', value: 'country_sector',
      choices: [['country_sector', 'Country-industry'], ['country', 'Country'], ['sector', 'Industry']] },
    { key: 'metric', type: 'radio', label: 'Rank by', value: 'share', inline: true, choices: [['share', '% of own GVA'], ['eur', 'EUR million']] },
    { key: 'min_va', type: 'number', label: 'Leave out country-industries with GVA below (EUR m)', value: D.min_va, min: 0, step: 50 },
    { key: 'top', type: 'number', label: 'Rows in the chart', value: D.top_n, min: 5, max: 60, step: 5 },
    { type: 'hr' },
    { key: 'amount', type: 'number', label: 'Scenario: change in this government’s consumption (EUR m)', value: 10000, step: 1000 },
    { type: 'button', label: 'Run in the shock simulator', onClick: () => {
      simRequest = { gov: S.gov, amount: S.amount, show: [...S.show] };
      location.hash = 'sim';
    } },
    { type: 'hint', html: 'Government consumption (P3_S13) covers public services (health, education, administration, defence services) and goods provided to households. Military equipment is investment (GFCF), not consumption: for defence procurement use the simulator with “Investment (GFCF)” and the defence-related industries.' }
  ], () => update());
  const S = ctl.state;
  const govColours = [['Home government', blue], ['Foreign governments', '#eb6834']];
  const cShare = card(content, { title: 'Share of GVA that depends on government consumption',
    note: 'Home = the country’s own government. Foreign = all other governments.' });
  const cBenef = card(content, { title: 'Who benefits from the consumption of a government' });
  const update = debounce(async () => {
    try {
      needSel(S.show);
      const sh = model.demandShares(S.show).sort((a, b) => (b.share_domestic + b.share_foreign) - (a.share_domestic + a.share_foreign));
      cShare.set({
        chart: w => stackedH(w, { rows: sh.flatMap(r => [{ label: r.country_name, category: 'Home government', value: r.share_domestic },
                                                        { label: r.country_name, category: 'Foreign governments', value: r.share_foreign }]),
                                  labels: sh.map(r => r.country_name), colours: govColours, xTitle: 'Share of the country’s GVA', caption: `${year}.` }),
        data: sh, file: 'government_share',
        columns: [COLS.country, { key: 'share_domestic', label: 'Home government', type: 'pct' }, { key: 'share_foreign', label: 'Foreign governments', type: 'pct' },
                  { key: 'va_domestic', label: 'Home, EUR m', type: 'eur' }, { key: 'va_foreign', label: 'Foreign, EUR m', type: 'eur' }, COLS.va]
      });
    } catch (e) { cShare.message(e.message, true); }
    try {
      const govName = model.countryNames[S.gov];
      cBenef.setTitle(`Who benefits from the consumption of the government of ${govName} (outside ${S.gov})`);
      needSel(S.show);
      const show = model.expandCountries(S.show).filter(c => c !== S.gov);
      if (!show.length) throw new Error('Choose countries other than the government’s own.');
      if (!model.L) cBenef.message('Loading the model data…');
      await ensure('L');
      const imp = model.fdShock({ buyers: [S.gov], categories: ['P3_S13'], pct: 100, decompose: false });
      const share = S.metric === 'share';
      const s = model.summariseImpact(imp, S.level, show, minVaOf(S.min_va))
        .map(r => ({ ...r, name: { country: r.country_name, country_sector: r.label, sector: r.sector_name }[S.level] }))
        .sort((a, b) => share ? b.share_va - a.share_va : b.dva - a.dva);
      const top = s.slice(0, topOf(S.top, D.top_n));
      cBenef.set({
        chart: w => barsH(w, { rows: top, label: d => d.name, value: d => share ? d.share_va : d.dva, percent: share,
                               xTitle: share ? 'Share of own GVA' : 'GVA from this government, EUR m',
                               caption: `${year}. Countries shown: ${model.describe(S.show)}${S.show.includes(S.gov) ? `, without ${S.gov}` : ''}.`,
                               tip: d => `${d.name}\n${fmt.pct(d.share_va, 3)} of its GVA\n${fmt.m(d.dva, 1)}` }),
        data: s, file: 'government_beneficiaries',
        columns: [{ key: 'name', label: 'Who', type: 'text' }, { key: 'share_va', label: 'Share of own GVA', type: 'pct' },
                  { key: 'dva', label: 'GVA from this government, EUR m', type: 'eur1' }, COLS.va]
      });
    } catch (e) { cBenef.message(e.message, true); }
  }, 150);
  return { update };
}

// ==== 5. Shock simulator ===========================================================================================
function buildSim() {
  const { side, content } = page('sim');
  const ctl = buildControls(side, [
    { key: 'type', type: 'radio', label: 'Type of shock', value: 'final_demand', inline: true,
      choices: [['final_demand', 'Final demand'], ['production', 'Production']] },
    { type: 'group', when: s => s.type === 'final_demand', items: [
      { key: 'buyers', type: 'multi', label: 'Buyers: countries', value: ['DE'], options: countryOpts },
      { key: 'cats', type: 'checks', label: 'Final demand categories', value: catChoices.map(c => c[0]), choices: catChoices },
      { key: 'pc', type: 'multi', label: 'Products made in', value: ['World'], options: countryOpts },
      { key: 'ps', type: 'multi', label: 'Products of industries', value: ['All sectors'], options: sectorOpts }] },
    { type: 'group', when: s => s.type === 'production', items: [
      { key: 'src_c', type: 'multi', label: 'Producers: countries', value: ['DE'], options: countryOpts },
      { key: 'src_s', type: 'multi', label: 'Producers: industries', value: ['C29'], options: sectorOpts }] },
    { key: 'unit', type: 'radio', label: 'Size given as', value: 'pct', inline: true, choices: [['pct', '% change'], ['eur', 'EUR million']] },
    { key: 'size', type: 'number', label: 'Size', value: -10 },
    { key: 'induced', type: 'checkbox', label: 'Add household spending of extra wages', value: false },
    { type: 'group', when: s => s.induced, items: [
      { key: 'rate', type: 'number', label: 'Consumer spending per EUR of extra wages (EUR)', value: D.spend_rate, min: 0, max: 1, step: 0.05 }] },
    { type: 'button', label: 'Run shock', primary: true, onClick: () => run() },
    { type: 'hr' },
    { key: 'show', type: 'multi', label: 'Show results for', value: [D.focus], options: countryOpts },
    { key: 'min_va', type: 'number', label: 'Leave out country-industries with GVA below (EUR m)', value: D.min_va, min: 0, step: 50 },
    { key: 'top', type: 'number', label: 'Rows in the chart', value: D.top_n, min: 5, max: 60, step: 5 },
    { type: 'hint', html: 'An amount in EUR is spread like current demand (or output). Effects are linear: twice the shock gives twice the effect. Household spending of extra wages: households in every country spend part of the extra wages, round after round (see Notes).' }
  ], key => { if (['show', 'min_va', 'top'].includes(key)) render(); });
  const S = ctl.state;
  const desc = el('div', { class: 'desc' });
  content.append(desc);
  const boxes = valueBoxes(content, [
    { key: 'size', title: 'Size of the shock' },
    { key: 'world', title: 'Change in world GVA' },
    { key: 'shown', title: 'Change in GVA of the countries shown' }]);
  let scope = 'shown', exclShocked = true;
  const excl = el('label', { class: 'check', hidden: true }, el('input', { type: 'checkbox', checked: true, onchange: e => { exclShocked = e.target.checked; render(); } }),
    ' Leave out the countries where the shock happens');
  const cCountry = card(content, { title: 'Change in GVA by country (% of the country’s GVA)',
    top: el('div', { class: 'card-switch-row' },
      inlineRadio([['shown', 'Countries shown'], ['world', '20 most affected worldwide']], 'shown', v => { scope = v; excl.hidden = v !== 'world'; render(); }), excl) });
  const cCs = card(content, { title: 'Most affected country-industries (% of their own GVA)',
    note: 'Direct = the producers of what is bought. First-round suppliers = what they buy as inputs. Further rounds = the suppliers’ suppliers, and so on. Household spending = spending of the extra wages (when switched on). In a production shock, the shocked producers themselves are left out of the chart.' });
  const cHeat = card(content, { title: 'Change in GVA by broad industry group (% of the group’s GVA)' });
  const dlAll = el('button', { class: 'btn', disabled: true, onclick: () => downloadAll() }, 'Download all results (Excel)');
  content.append(el('div', { class: 'dl-all' }, dlAll));

  let spec = null, imp = null, ran = false;
  const cards = [cCountry, cCs, cHeat];
  function shockText(sp) {
    const size = sp.unit === 'pct' ? `${sp.size > 0 ? '+' : ''}${sp.size}%` : `${sp.size > 0 ? '+' : ''}${fmt.num(sp.size)} EUR m`;
    const txt = sp.type === 'final_demand'
      ? `Final demand of ${model.describe(sp.buyers)} (${sp.cats.map(k => model.catLabels[k]).join(', ')}) for products of ${model.describe(sp.pc, sp.ps)}: ${size}, ${year}.`
      : `Production of ${model.describe(sp.src_c, sp.src_s)}: ${size}, ${year}.`;
    return sp.induced ? `${txt} ${induceText(sp.rate)}` : txt;
  }
  async function run() {
    ran = true;
    spec = JSON.parse(JSON.stringify(S));
    try {
      checkSize(spec.size);
      if (spec.induced) checkSpendRate(spec.rate);
      if (spec.type === 'final_demand') { needSel(spec.buyers, spec.cats, spec.pc, spec.ps); } else needSel(spec.src_c, spec.src_s);
      desc.textContent = shockText(spec);
      if (!model.L || !model.A) cards.forEach(c => c.message('Loading the model data…'));
      await ensure('L', 'A');
      const size = spec.unit === 'pct' ? { pct: spec.size } : { eur: spec.size };
      const common = { ...size, induced: spec.induced, spendRate: spec.induced ? spec.rate : D.spend_rate };
      imp = spec.type === 'final_demand'
        ? model.fdShock({ buyers: spec.buyers, categories: spec.cats, productCountries: spec.pc, productSectors: spec.ps, ...common })
        : model.outputShock({ sourceCountries: spec.src_c, sourceSectors: spec.src_s, ...common });
      render();
    } catch (e) { imp = null; dlAll.disabled = true; desc.textContent = ''; cards.forEach(c => c.message(e.message, true)); }
  }
  function render() {
    if (!imp) return;
    try {
      needSel(S.show);
      const prod = imp.what === 'production';
      const world = model.summariseImpact(imp, 'total')[0], shown = model.summariseImpact(imp, 'total', S.show)[0];
      const hhNote = t => imp.induced ? `of which household spending: ${fmt.amount(t.dva_induced)}` : '';
      boxes.set('size', fmt.amount(imp.size));
      boxes.set('world', `${fmt.amount(world.dva)} (${fmt.pct(world.share_va, 3)})`, hhNote(world));
      boxes.set('shown', `${fmt.amount(shown.dva)} (${fmt.pct(shown.share_va, 3)})`, hhNote(shown));
      const showSet = new Set(model.expandCountries(S.show));

      let countries = model.summariseImpact(imp, 'country').sort((a, b) => a.share_va - b.share_va);
      let shownC;
      if (scope === 'shown') shownC = countries.filter(r => showSet.has(r.country));
      else {
        const shocked = new Set(prod ? model.countryCodes.filter((c, ci) => imp.inSource.some((v, i) => v && model.cOf(i) === ci))
                                     : model.expandCountries(spec.buyers));
        const rest = countries.filter(r => !shocked.has(r.country));
        shownC = (exclShocked && rest.length ? rest : countries).sort((a, b) => Math.abs(b.share_va) - Math.abs(a.share_va)).slice(0, 20);
      }
      shownC = [...shownC].sort((a, b) => Math.abs(b.share_va) - Math.abs(a.share_va));
      cCountry.set({
        chart: w => barsH(w, { rows: shownC, label: d => d.country_name, value: d => d.share_va, xTitle: 'Change in GVA, % of the country’s GVA',
                               highlight: scope === 'world' ? d => showSet.has(d.country) : null, highlightNames: ['Countries shown', 'Other'],
                               tip: d => `${d.country_name}\n${fmt.pct(d.share_va, 3)} of GVA\n${fmt.m(d.dva, 1)}` }),
        data: countries, file: 'shock_by_country',
        columns: [COLS.country, { key: 'share_va', label: 'Change, % of GVA', type: 'pct' }, { key: 'dva', label: 'Change in GVA, EUR m', type: 'eur1' },
                  ...(prod ? [{ key: 'dva_excl_source', label: 'Of which outside the shocked producers, EUR m', type: 'eur1' }] : []),
                  { key: 'dva_direct', label: 'Direct, EUR m', type: 'eur1' }, { key: 'dva_first', label: 'First-round suppliers, EUR m', type: 'eur1' },
                  { key: 'dva_further', label: 'Further supply-chain rounds, EUR m', type: 'eur1' },
                  ...(imp.induced ? [{ key: 'dva_induced', label: 'Household spending, EUR m', type: 'eur1' }] : []), COLS.va]
      });

      const cs = model.summariseImpact(imp, 'country_sector', S.show, minVaOf(S.min_va)).sort((a, b) => a.share_va - b.share_va);
      const csChart = cs.filter(r => !(prod && r.in_source)).sort((a, b) => Math.abs(b.share_va) - Math.abs(a.share_va)).slice(0, topOf(S.top, D.top_n))
        .map(r => ({ ...r, direct: r.dva_direct, first: r.dva_first, further: r.dva_further, induced: r.dva_induced }));
      cCs.setParts(partsText('in the countries shown', shown.dva, shown.dva_direct, shown.dva_first, shown.dva_further, shown.dva_induced, prod, imp.induced));
      if (!csChart.length) cCs.message('No country-industries above the GVA threshold.');
      else cCs.set({
        chart: w => stackedH(w, { rows: effectRows(csChart, d => d.label, true), labels: csChart.map(d => d.label), colours: effectColours,
                                  xTitle: 'Change in GVA, % of own GVA' }),
        data: cs, file: 'shock_by_country_industry',
        columns: [{ key: 'label', label: 'Country-industry', type: 'text' }, { key: 'share_va', label: 'Change, % of own GVA', type: 'pct' },
                  { key: 'dva', label: 'Change in GVA, EUR m', type: 'eur1' }, { key: 'dva_direct', label: 'Direct, EUR m', type: 'eur1' },
                  { key: 'dva_first', label: 'First-round suppliers, EUR m', type: 'eur1' }, { key: 'dva_further', label: 'Further supply-chain rounds, EUR m', type: 'eur1' },
                  ...(imp.induced ? [{ key: 'dva_induced', label: 'Household spending, EUR m', type: 'eur1' }] : []),
                  { key: 'share_country_va', label: 'Change, % of country’s GVA', type: 'pct' }, COLS.va]
      });

      const heat = model.summariseImpact(imp, 'country_sector_group', S.show);
      const codes = model.expandCountries(S.show);
      const long = heat.map(r => ({ row: r.sector_group, col: r.country, value: r.share_va }));
      const wide = meta.broad_groups.map(g => Object.fromEntries([['sector_group', g], ...codes.map(c => [c, heat.find(r => r.sector_group === g && r.country === c)?.share_va])]));
      cHeat.set({
        chart: w => heatmap(w, { rows: long, rowLevels: meta.broad_groups, colLevels: codes, digits: 2, caption: 'Values in % of the group’s GVA in each country.' }),
        data: wide, file: 'shock_by_industry_group',
        columns: [{ key: 'sector_group', label: 'Industry group', type: 'text' }, ...codes.map(c => ({ key: c, label: c, type: 'pct' }))]
      });
      dlAll.disabled = false;
    } catch (e) { cards.forEach(c => c.message(e.message, true)); }
  }
  async function downloadAll() {
    const cs = model.summariseImpact(imp, 'country_sector');
    const csCols = ['country', 'country_name', 'sector', 'sector_name', 'va', 'dva', 'dva_direct', 'dva_first', 'dva_further', 'dva_induced', 'share_va', 'share_country_va', 'in_source']
      .map(k => ({ key: k, label: k }));
    const cCols = ['country', 'country_name', 'va', 'dva', 'dva_excl_source', 'dva_direct', 'dva_first', 'dva_further', 'dva_induced', 'share_va'].map(k => ({ key: k, label: k }));
    const g = model.summariseImpact(imp, 'country_sector_group', S.show);
    const gCols = ['country', 'sector_group', 'va', 'dva', 'share_va'].map(k => ({ key: k, label: k }));
    await downloadXlsx([{ name: 'shock', aoa: [['shock'], [shockText(spec)]] },
                        { name: 'by_country', rows: model.summariseImpact(imp, 'country'), cols: cCols },
                        { name: 'by_country_industry', rows: cs, cols: csCols },
                        { name: 'by_group_shown', rows: g, cols: gCols }], 'shock_results.xlsx');
  }
  // A scenario from the Government tab: set the controls and run it
  function takeRequest(r) {
    ctl.set.type('final_demand'); ctl.set.buyers([r.gov]); ctl.set.cats(['P3_S13']); ctl.set.pc(['World']);
    ctl.set.ps(['All sectors']); ctl.set.unit('eur'); ctl.set.size(r.amount); ctl.set.show(r.show);
    ctl.refresh();
  }
  return {
    update: () => {
      if (simRequest) { takeRequest(simRequest); simRequest = null; run(); }
      else if (!ran) run();
    }
  };
}

// ==== 4. Domestic shock ============================================================================================
function buildDom() {
  const { side, content } = page('dom');
  const ctl = buildControls(side, [
    { key: 'c', type: 'select', label: 'Country', value: D.origin, choices: meta.countries.map(c => [c.code, c.name]) },
    { type: 'hr' },
    { key: 'type', type: 'radio', label: 'Shock to the country’s own', value: 'final_demand', inline: true,
      choices: [['final_demand', 'Final demand'], ['production', 'Production']] },
    { type: 'group', when: s => s.type === 'final_demand', items: [
      { key: 'cats', type: 'checks', label: 'Final demand categories', value: ['P3_S13'], choices: catChoices },
      { key: 'fd_s', type: 'multi', label: 'Products of industries', value: ['All sectors'], options: sectorOpts },
      { key: 'from', type: 'radio', label: 'Products bought', value: 'all',
        choices: [['all', 'Home-made and imported, as now'], ['home', 'Home-made only']] }] },
    { type: 'group', when: s => s.type === 'production', items: [
      { key: 'prod_s', type: 'multi', label: 'Home industries producing', value: ['F'], options: sectorOpts }] },
    { key: 'unit', type: 'radio', label: 'Size given as', value: 'eur', inline: true, choices: [['eur', 'EUR million'], ['pct', '% change']] },
    { key: 'size', type: 'number', label: 'Size', value: 1000 },
    { key: 'induced', type: 'checkbox', label: 'Add household spending of extra wages', value: false },
    { type: 'group', when: s => s.induced, items: [
      { key: 'rate', type: 'number', label: 'Consumer spending per EUR of extra wages (EUR)', value: D.spend_rate, min: 0, max: 1, step: 0.05 }] },
    { type: 'hr' },
    { key: 'level', type: 'radio', label: 'Results by', value: 'sector', inline: true, choices: [['sector', 'Industry'], ['sector_group', 'Broad industry group']] },
    { key: 'min_va', type: 'number', label: 'Leave out industries with GVA below (EUR m)', value: D.min_va, min: 0, step: 50 },
    { key: 'top', type: 'number', label: 'Rows in the chart', value: D.top_n, min: 5, max: 60, step: 5 },
    { type: 'hint', html: '<b>Final demand:</b> the country’s own households, government, investors, … change their purchases. “As now” keeps their import shares, so part of the shock goes straight abroad.<br><b>Production:</b> the output of home industries changes; their suppliers at home and abroad follow.<br><b>Household spending:</b> households spend part of the extra wages, round after round. Without it, effects are a lower bound; with it, the upper end of a pure demand effect (no prices, interest rates or crowding out).' }
  ], key => {
    if (key === 'unit') ctl.set.size((S.size < 0 ? -1 : 1) * (S.unit === 'pct' ? 10 : 1000));
    update();
  });
  const S = ctl.state;
  content.append(el('h2', { class: 'section' }, 'The shock'));
  const desc = el('div', { class: 'desc' });
  content.append(desc);
  const boxes = valueBoxes(content, [
    { key: 'size', title: 'Size of the shock' }, { key: 'home', title: 'Change in home GVA' },
    { key: 'mult', title: 'Home GVA per EUR 1 of shock' }, { key: 'abroad', title: 'Change in GVA abroad' }]);
  let metric = 'eur', multOrder = 'high';
  const cInd = card(content, { title: 'Change in GVA of home industries',
    top: inlineRadio([['eur', 'EUR million'], ['share', '% of own GVA']], 'eur', v => { metric = v; update(); }),
    note: 'Direct = the industries whose products are bought. First-round suppliers = what they buy as inputs. Further rounds = the suppliers’ suppliers, and so on. Household spending = spending of the extra wages (when switched on). In a production shock, the shocked industries themselves are left out (they change by exactly the shock); by industry group, only the change outside them is counted.' });
  const cAbroad = card(content, { title: 'Change in GVA abroad, by country (leakage through imports)' });
  content.append(el('h2', { class: 'section' }, 'How the home economy is built'),
                 el('div', { class: 'hint' }, 'These views need no shock: they show the country’s structure in the chosen year.'));
  const cExpo = card(content, { title: 'Share of each home industry’s GVA that depends on each kind of final demand (%)',
    note: 'Home final demand by category, and all foreign final demand. Each row adds up to 100%. A 10% fall in a category of home demand lowers the industry’s GVA by 10% of its share (before household spending of the lost wages).' });
  const cContent = card(content, { title: 'Where EUR 1 of home final demand ends up',
    note: 'All products bought by the country’s own buyers, home-made and imported, at basic prices. Taxes on products & other = taxes less subsidies on products paid on inputs along the chain. Inventories are in the table only (small, can be negative). With household spending on, the green part is the extra home GVA it brings, on top of the EUR 1.' });
  const cMult = card(content, { title: 'Where EUR 1 of final demand for a home industry’s products ends up',
    top: inlineRadio([['high', 'Most stays at home'], ['low', 'Least stays at home']], 'high', v => { multOrder = v; update(); }),
    note: 'Per EUR 1 of final demand for the industry’s products, whoever buys them. Other home industries = its home supply chain. The table also has the output multiplier (home output per EUR). With household spending on, the green part is the extra home GVA it brings, on top of the EUR 1.' });
  const shockCards = [cInd, cAbroad], structCards = [cExpo, cContent, cMult];

  const update = debounce(async () => {
    const c = S.c, cname = model.countryNames[c], what = `${cname}, ${year}.`;
    const top = topOf(S.top, D.top_n), minVA = minVaOf(S.min_va);
    let rate = D.spend_rate;
    try {
      if (S.induced) rate = checkSpendRate(S.rate);
      if (!model.L || !model.A) [...shockCards, cContent, cMult].forEach(x => x.message('Loading the model data…'));
      await ensure('L', 'A');
    } catch (e) { [...shockCards, ...structCards].forEach(x => x.message(e.message, true)); return; }

    // -- The shock
    try {
      checkSize(S.size);
      const fd = S.type === 'final_demand';
      if (fd) { needSel(S.cats, S.fd_s); } else needSel(S.prod_s);
      const size = S.unit === 'pct' ? { pct: S.size } : { eur: S.size };
      const imp = model.domesticShock({ country: c, type: S.type, categories: S.cats, sectors: fd ? S.fd_s : S.prod_s,
                                        productsFrom: S.from, ...size, induced: S.induced, spendRate: rate });
      const h = model.homeVsAbroad(imp, c), prod = !fd;
      const sizeTxt = S.unit === 'pct' ? `${S.size > 0 ? '+' : ''}${S.size}%` : `${S.size > 0 ? '+' : ''}${fmt.num(S.size)} EUR m`;
      const txt = fd
        ? `${cname}: final demand of home buyers (${S.cats.map(k => model.catLabels[k]).join(', ')}) for ${S.from === 'home' ? 'home-made' : 'home-made and imported'} products of ${describeSectors(S.fd_s)}: ${sizeTxt}, ${year}.`
        : `${cname}: production of ${describeSectors(S.prod_s)}: ${sizeTxt}, ${year}.`;
      desc.textContent = S.induced ? `${txt} ${induceText(rate)}` : txt;
      boxes.set('size', fmt.amount(h.shock), prod ? 'output of the shocked home industries' : 'final demand, basic prices');
      boxes.set('home', fmt.amount(h.dva_home), `${fmt.pct(h.share_home_va, 3)} of the country’s GVA${S.induced ? `; of which household spending: ${fmt.amount(h.dva_home_induced)}` : ''}`);
      boxes.set('mult', fmt.eur(h.home_per_eur), S.induced ? `without household spending: ${fmt.eur(h.home_per_eur_type1)}`
        : prod ? `of which other home industries: ${fmt.eur(h.dva_home_other / h.shock)}` : `abroad: ${fmt.eur(h.abroad_per_eur)}; the rest is taxes on products`);
      boxes.set('abroad', fmt.amount(h.dva_abroad), `${fmt.pct(h.leakage)} of the change in GVA everywhere`);

      let s = model.summariseImpact(imp, S.level, [c], minVA);
      if (prod && S.level === 'sector') s = s.filter(r => !r.in_source);
      s = s.map(r => ({ ...r, name: S.level === 'sector' ? r.sector_name : r.sector_group,
                        change: prod ? r.dva_excl_source : r.dva, direct: prod ? 0 : r.dva_direct, first: r.dva_first,
                        further: r.dva_further, induced: prod ? r.dva_induced_excl_source : r.dva_induced }))
           .map(r => ({ ...r, share: r.change / r.va }));
      s.sort((a, b) => Math.abs(metric === 'share' ? b.share : b.change) - Math.abs(metric === 'share' ? a.share : a.change));
      const sTop = s.slice(0, top);
      cInd.setParts(partsText(`in ${cname}`, h.dva_home, h.dva_home_direct, h.dva_home_first, h.dva_home_further, h.dva_home_induced, prod, S.induced));
      if (!sTop.length) cInd.message('No home industries above the GVA threshold.');
      else cInd.set({
        chart: w => stackedH(w, { rows: effectRows(sTop, d => d.name, metric === 'share'), labels: sTop.map(d => d.name), colours: effectColours,
                                  percent: metric === 'share', xTitle: metric === 'share' ? 'Change in GVA, % of own GVA' : 'Change in GVA, EUR m', caption: what }),
        data: s, file: 'domestic_shock_home_industries',
        columns: [{ key: 'name', label: nameCol(S.level), type: 'text' }, { key: 'change', label: 'Change in GVA, EUR m', type: 'eur1' },
                  ...(prod ? [] : [{ key: 'direct', label: 'Direct, EUR m', type: 'eur1' }]),
                  { key: 'first', label: 'First-round suppliers, EUR m', type: 'eur1' }, { key: 'further', label: 'Further supply-chain rounds, EUR m', type: 'eur1' },
                  ...(S.induced ? [{ key: 'induced', label: 'Household spending, EUR m', type: 'eur1' }] : []),
                  { key: 'share', label: 'Change, % of own GVA', type: 'pct' }, COLS.va]
      });

      const ab = model.summariseImpact(imp, 'country').filter(r => r.country !== c);
      const totalAb = sum(ab.map(r => r.dva));
      ab.forEach(r => { r.share_abroad = r.dva / totalAb; });
      ab.sort((a, b) => Math.abs(b.dva) - Math.abs(a.dva));
      const focus = new Set(model.expandCountries([D.focus]));
      cAbroad.set({
        chart: w => barsH(w, { rows: ab.slice(0, top), label: d => d.country_name, value: d => d.dva, percent: false, xTitle: 'Change in GVA, EUR m',
                               highlight: d => focus.has(d.country), highlightNames: [D.focus, 'Other countries'],
                               caption: `All countries abroad: ${fmt.m(h.dva_abroad)}, ${fmt.pct(h.leakage)} of the change in GVA everywhere.`,
                               tip: d => `${d.country_name}\n${fmt.m(d.dva, 1)}\n${fmt.pct(d.share_abroad)} of the change abroad` }),
        data: ab, file: 'domestic_shock_abroad',
        columns: [COLS.country, { key: 'dva', label: 'Change in GVA, EUR m', type: 'eur1' }, { key: 'share_abroad', label: 'Share of the change abroad', type: 'pct' },
                  { key: 'share_va', label: 'Change, % of the country’s GVA', type: 'pct' }, COLS.va]
      });
    } catch (e) { desc.textContent = ''; shockCards.forEach(x => x.message(e.message, true)); }

    // -- The structure of the home economy
    try {
      const short = { P3_S14: 'Households', P3_S13: 'Government', P3_S15: 'NPISH', P51G: 'Investment', P5M: 'Inventories', FOREIGN: 'Abroad' };
      const e = model.homeDemandExposure(c, S.level);
      const sizes = [...new Map(e.map(r => [r.origin_name, r.va]))].sort((a, b) => b[1] - a[1]).filter(([, va]) => S.level !== 'sector' || va >= minVA);
      const rowNames = S.level === 'sector' ? sizes.slice(0, top).map(x => x[0]) : sizes.map(x => x[0]);
      const cols = Object.values(short);
      const long = e.filter(r => rowNames.includes(r.origin_name)).map(r => ({ row: r.origin_name, col: short[r.code], value: r.share }));
      const wide = sizes.map(([name, va]) => Object.fromEntries([['origin_name', name], ['va', va],
        ...e.filter(r => r.origin_name === name).map(r => [short[r.code], r.share])]));
      cExpo.set({
        chart: w => heatmap(w, { rows: long, rowLevels: rowNames, colLevels: cols,
                                 caption: `${what} % of the row’s GVA.${S.level === 'sector' ? ' Largest industries by GVA; the table has all.' : ''}` }),
        data: wide, file: 'home_industries_by_demand',
        columns: [{ key: 'origin_name', label: nameCol(S.level), type: 'text' }, COLS.va, ...cols.map(k => ({ key: k, label: k, type: 'pct' }))]
      });

      const content_ = model.domesticContent(c, S.induced, rate);
      const parts = [['home_per_eur', 'Home GVA'], ['abroad_per_eur', 'GVA abroad'], ['other_per_eur', 'Taxes on products & other'],
                     ...(S.induced ? [['home_hh_per_eur', hhLabel]] : [])];
      const contentRows = content_.filter(r => r.code !== 'P5M').flatMap(r => parts.map(([k, name]) => ({ label: r.category, category: name, value: r[k] })));
      cContent.set({
        chart: w => stackedH(w, { rows: contentRows, labels: content_.filter(r => r.code !== 'P5M').map(r => r.category),
                                  colours: [...contentColours, [hhLabel, hhGreen]], xTitle: 'Share of each EUR of final demand', caption: what }),
        data: content_, file: 'home_demand_content',
        columns: [{ key: 'category', label: 'Final demand', type: 'text' }, { key: 'final_demand', label: 'Final demand, EUR m', type: 'eur' },
                  { key: 'va_home', label: 'Home GVA, EUR m', type: 'eur' }, { key: 'home_per_eur', label: 'Home GVA, % of each EUR', type: 'pct' },
                  { key: 'abroad_per_eur', label: 'GVA abroad, % of each EUR', type: 'pct' },
                  { key: 'other_per_eur', label: 'Taxes on products & other, % of each EUR', type: 'pct' },
                  ...(S.induced ? [{ key: 'home_hh_per_eur', label: 'Extra home GVA from household spending, % of each EUR', type: 'pct' }] : [])]
      });

      const m = model.domesticMultipliers(c, S.induced, rate).filter(r => r.va >= minVA && Number.isFinite(r.va_home))
        .map(r => ({ ...r, total_home: r.va_home + (S.induced ? r.va_home_hh : 0) }));
      m.sort((a, b) => multOrder === 'high' ? b.total_home - a.total_home : a.total_home - b.total_home);
      const mTop = m.slice(0, top);
      const mParts = [['va_own', 'The industry itself'], ['va_other_home', 'Other home industries'], ['va_abroad', 'GVA abroad'],
                      ['other', 'Taxes on products & other'], ...(S.induced ? [['va_home_hh', hhLabel]] : [])];
      if (!mTop.length) cMult.message('No home industries above the GVA threshold.');
      else cMult.set({
        chart: w => stackedH(w, { rows: mTop.flatMap(r => mParts.map(([k, name]) => ({ label: r.sector_name, category: name, value: r[k] }))),
                                  labels: mTop.map(r => r.sector_name), colours: [...multiplierColours, [hhLabel, hhGreen]],
                                  xTitle: 'Share of each EUR of final demand', caption: `${what} Industries with GVA of at least ${fmt.m(minVA)}.` }),
        data: m, file: 'home_multipliers',
        columns: [{ key: 'sector_name', label: 'Industry', type: 'text' }, { key: 'va_home', label: 'Home GVA, % of each EUR', type: 'pct' },
                  { key: 'va_own', label: 'Of which the industry itself', type: 'pct' }, { key: 'va_other_home', label: 'Of which other home industries', type: 'pct' },
                  { key: 'va_abroad', label: 'GVA abroad, % of each EUR', type: 'pct' }, { key: 'other', label: 'Taxes & other, % of each EUR', type: 'pct' },
                  ...(S.induced ? [{ key: 'va_home_hh', label: 'Extra home GVA from household spending, % of each EUR', type: 'pct' }] : []),
                  { key: 'output_multiplier', label: 'Home output per EUR (supply chain only)', type: 'dec' },
                  { key: 'final_demand', label: 'Final demand for its products, EUR m', type: 'eur' }, COLS.va]
      });
    } catch (e) { structCards.forEach(x => x.message(e.message, true)); }
  }, 150);
  return { update };
}

// ==== Notes ======================================================================================================
function buildNotes() {
  const { content } = page('notes', false);
  const box = el('div', { class: 'notes' }, message('Loading…'));
  content.append(box);
  let loaded = false;
  return {
    async update() {
      if (loaded) return;
      loaded = true;
      const html = await (await fetch('notes.html')).text();
      box.innerHTML = `<p class="lead">This website runs the input-output model for ${year} in your browser: nothing is sent to a server. The full version (R and Shiny, all years 2010–2024) is in the project’s repository.</p>${html}`;
    }
  };
}

// ==== Tabs and start-up ==========================================================================================
const TABS = [
  { id: 'dest', title: 'Where VA ends up', build: buildDest },
  { id: 'expo', title: 'Exposure to a source', build: buildExpo },
  { id: 'src', title: 'Sources of an origin', build: buildSrc },
  { id: 'pairs', title: 'Top pairs', build: buildPairs },
  { id: 'sim', title: 'Shock simulator', build: buildSim },
  { id: 'dom', title: 'Domestic shock', build: buildDom },
  { id: 'gov', title: 'Government', build: buildGov },
  { id: 'notes', title: 'Notes', build: buildNotes }
];
const built = {};
function showTab(id) {
  if (!TABS.some(t => t.id === id)) id = 'dest';
  for (const t of TABS) {
    const sec = document.getElementById(`page-${t.id}`);
    if (sec) sec.hidden = t.id !== id;
    document.getElementById(`tab-${t.id}`)?.classList.toggle('active', t.id === id);
  }
  if (!built[id]) built[id] = TABS.find(t => t.id === id).build();
  document.getElementById(`page-${id}`).hidden = false;
  built[id].update();
}

async function start() {
  const years = await (await fetch('data/years.json')).json();
  year = years.latest;
  base = `data/${year}`;
  meta = await (await fetch(`${base}/meta.json`)).json();
  D = meta.defaults;
  const core = parseCore(meta, await fetchBuffer(`${base}/${meta.files.core}`, 'the core tables', meta.bytes.core));
  model = new IOModel(meta, core);
  pickerOptions();
  document.getElementById('data-note').textContent = `Eurostat FIGARO (${meta.edition}), ${year}, EUR million, current prices`;
  const nav = document.getElementById('tabs');
  nav.replaceChildren(...TABS.map(t => el('a', { id: `tab-${t.id}`, href: `#${t.id}` }, t.title)));
  document.getElementById('loading').remove();
  window.addEventListener('hashchange', () => showTab(location.hash.slice(1)));
  showTab(location.hash.slice(1) || 'dest');
}
start().catch(e => showError(`Could not start: ${e.message}`));
