// =============================================================================
// Engine: the input-output model in the browser. A port of R/engine.R (same logic,
// same results up to 4-byte rounding). No DOM code here, so it also runs in Node for tests.
//
// All values in EUR million, shares 0-1. Matrices are Float32Arrays stored column by column,
// as R stores them: element (i, j) of a matrix with n rows is M[i + j * n].
// Country-industry i = country * nSectors + sector; final demand column j = buyer * nCategories + category.
// =============================================================================

export const SMALL_SOURCE = 400;   // production shocks to more country-industries than this use A (iteration)

export class IOModel {
  constructor(meta, core) {
    this.meta = meta;
    this.nc = meta.n_countries; this.ns = meta.n_sectors; this.n = this.nc * this.ns;
    this.nk = meta.fd_categories.length; this.nfd = this.nc * this.nk;
    Object.assign(this, core);          // x, va, v, wage, gos, Y, VAD, sales, LH, WLH, basic
    this.L = null; this.A = null;
    this.countryIndex = new Map(meta.countries.map((c, i) => [c.code, i]));
    this.sectorIndex = new Map(meta.sectors.map((s, i) => [s.code, i]));
    this.countryCodes = meta.countries.map(c => c.code);
    this.countryNames = Object.fromEntries(meta.countries.map(c => [c.code, c.name]));
    this.sectorCodes = meta.sectors.map(s => s.code);
    this.sectorNames = Object.fromEntries(meta.sectors.map(s => [s.code, s.name]));
    this.sectorGroupOf = meta.sectors.map(s => s.group);
    this.catCodes = meta.fd_categories.map(k => k.code);
    this.catLabels = Object.fromEntries(meta.fd_categories.map(k => [k.code, k.label]));
  }

  // ---- Labels ------------------------------------------------------------------------------
  cOf(i) { return Math.floor(i / this.ns); }
  sOf(i) { return i % this.ns; }
  countryOf(i) { return this.countryCodes[this.cOf(i)]; }
  sectorOf(i) { return this.sectorCodes[this.sOf(i)]; }
  shortCode(code) { return code === 'FIGW1' ? 'RoW' : code; }
  label(i) { return `${this.shortCode(this.countryOf(i))} ${this.sectorNames[this.sectorOf(i)]}`; }
  homeRows(code) { const c = this.countryIndex.get(code); return range(c * this.ns, (c + 1) * this.ns); }

  // A short readable name for a selection, e.g. "Poland, Manufacturing"
  describe(countries, sectors = ['All sectors']) {
    const c = countries.map(x => this.countryNames[x] ?? x).join(' + ');
    const s = sectors.map(x => this.sectorNames[x] ?? x).join(' + ');
    return sectors.length === 1 && sectors[0] === 'All sectors' ? c : `${c}, ${s}`;
  }

  // ---- Selections --------------------------------------------------------------------------
  expandCountries(list) {
    const out = new Set();
    for (const e of list) {
      const g = this.meta.country_groups[e];
      if (g) g.forEach(c => out.add(c));
      else if (this.countryIndex.has(e)) out.add(e);
      else throw new Error(`Unknown country or group: ${e}`);
    }
    return [...out];
  }
  expandSectors(list) {
    const out = new Set();
    for (const e of list) {
      const g = this.meta.sector_groups[e];
      if (g) g.forEach(s => out.add(s));
      else if (this.sectorIndex.has(e)) out.add(e);
      else throw new Error(`Unknown industry or group: ${e}`);
    }
    return [...out];
  }
  expandCategories(list) {
    if (list.length === 1 && list[0] === 'All') return [...this.catCodes];
    for (const k of list) if (!this.catCodes.includes(k)) throw new Error(`Unknown final demand category: ${k}`);
    return list;
  }
  // Row numbers of the chosen country-industries
  selectCS(countries = ['World'], sectors = ['All sectors']) {
    const cs = new Set(this.expandCountries(countries).map(c => this.countryIndex.get(c)));
    const ss = new Set(this.expandSectors(sectors).map(s => this.sectorIndex.get(s)));
    const rows = [];
    for (let i = 0; i < this.n; i++) if (cs.has(this.cOf(i)) && ss.has(this.sOf(i))) rows.push(i);
    return rows;
  }
  // Column numbers of the chosen final demand (buyer countries x categories)
  selectFD(buyers = ['World'], categories = ['All']) {
    const bs = new Set(this.expandCountries(buyers).map(c => this.countryIndex.get(c)));
    const ks = new Set(this.expandCategories(categories).map(k => this.catCodes.indexOf(k)));
    const cols = [];
    for (let j = 0; j < this.nfd; j++) if (bs.has(Math.floor(j / this.nk)) && ks.has(j % this.nk)) cols.push(j);
    return cols;
  }

  requireL() { if (!this.L) throw new Error('The model data is still loading.'); }
  requireA() { if (!this.A) throw new Error('The input coefficients are still loading.'); }
  // Does a production shock to these rows need the input coefficients?
  needsA(rows) { return rows.length > SMALL_SOURCE; }

  // ---- 1. Where an origin's value added ends up ------------------------------------------------
  // One row per (origin, buyer, category). level: "total", "country", "sector" or "country_sector".
  vaByBuyer(originCountries, originSectors = ['All sectors'], level = 'total') {
    const { n, nfd, nk } = this;
    const rows = this.selectCS(originCountries, originSectors);
    const own = new Set(this.expandCountries(originCountries));
    const keyOf = i => level === 'total' ? 'Selection' : level === 'country' ? this.countryOf(i)
      : level === 'sector' ? this.sectorOf(i) : `${this.countryOf(i)}_${this.sectorOf(i)}`;
    const agg = new Map();
    for (const i of rows) {
      const key = keyOf(i);
      let a = agg.get(key);
      if (!a) { a = { va: new Float64Array(nfd), originVA: 0 }; agg.set(key, a); }
      a.originVA += this.va[i];
      for (let j = 0; j < nfd; j++) a.va[j] += this.VAD[i + j * n];
    }
    const out = [];
    for (const [origin, a] of agg) {
      for (let j = 0; j < nfd; j++) {
        const buyer = this.countryCodes[Math.floor(j / nk)], code = this.catCodes[j % nk];
        const domestic = (level === 'country' || level === 'country_sector') ? buyer === origin.split('_')[0] : own.has(buyer);
        out.push({ origin, buyer, buyer_name: this.countryNames[buyer], code, category: this.catLabels[code],
                   va: a.va[j], origin_va: a.originVA, share: a.va[j] / a.originVA, domestic });
      }
    }
    return out;
  }

  // ---- 2. Direct exports vs value added absorbed, by foreign buyer ------------------------------------
  exportsVsVa(originCountries, originSectors = ['All sectors']) {
    const { n, nc, nk } = this;
    const rows = this.selectCS(originCountries, originSectors);
    const own = new Set(this.expandCountries(originCountries));
    const out = [];
    for (let b = 0; b < nc; b++) {
      const buyer = this.countryCodes[b];
      if (own.has(buyer)) continue;
      let gross = 0, vaAbs = 0;
      for (const i of rows) {
        gross += this.sales[i + b * n];
        for (let k = 0; k < nk; k++) vaAbs += this.VAD[i + (b * nk + k) * n];
      }
      out.push({ buyer, buyer_name: this.countryNames[buyer], gross_exports: gross, va_absorbed: vaAbs });
    }
    const g = sum(out.map(r => r.gross_exports)), v = sum(out.map(r => r.va_absorbed));
    out.forEach(r => { r.exports_share = r.gross_exports / g; r.va_share = r.va_absorbed / v; });
    return out.sort((a, b) => b.va_absorbed - a.va_absorbed);
  }

  // ---- 3. Shocks ------------------------------------------------------------------------------------
  // out += M[, col] * a  (M: n rows, column by column)
  addCol(M, col, a, out) {
    const n = this.n, off = col * n;
    for (let i = 0; i < n; i++) out[i] += M[off + i] * a;
  }
  // A[, cols] %*% vals with the sparse input coefficients
  aTimes(cols, vals) {
    const out = new Float64Array(this.n), { ptr, idx, val } = this.A;
    cols.forEach((j, r) => {
      const a = vals[r];
      if (!a) return;
      for (let p = ptr[j]; p < ptr[j + 1]; p++) out[idx[p]] += val[p] * a;
    });
    return out;
  }

  // An "impact": per country-industry the change in GVA (dva) and its parts. Like new_impact() in R.
  newImpact(dx, sourceRows, size, what, direct, first, induced, spendRate) {
    const n = this.n, v = this.v;
    const imp = { what, size, induced: induced !== null, spendRate, decomposed: direct !== null,
                  dva: new Float64Array(n), direct: new Float64Array(n), first: new Float64Array(n),
                  further: new Float64Array(n), dvaInduced: new Float64Array(n), inSource: new Uint8Array(n) };
    for (let i = 0; i < n; i++) {
      const ind = induced ? induced[i] : 0;
      imp.dva[i] = v[i] * (dx[i] + ind);
      imp.dvaInduced[i] = v[i] * ind;
      if (direct) {
        imp.direct[i] = v[i] * direct[i];
        imp.first[i] = v[i] * first[i];
        imp.further[i] = v[i] * (dx[i] - direct[i] - first[i]);
      } else {
        imp.direct[i] = imp.first[i] = imp.further[i] = NaN;
      }
    }
    for (const i of sourceRows) imp.inSource[i] = 1;
    return imp;
  }

  // Final demand shock: buyers x categories x products (country-industries that make them).
  // pct: % change of that demand; eur: amount in EUR million spread like current demand.
  fdShock({ buyers = ['World'], categories = ['All'], productCountries = ['World'], productSectors = ['All sectors'],
            pct = null, eur = null, induced = false, spendRate = this.meta.defaults.spend_rate, decompose = true }) {
    this.requireL();
    if ((pct === null) === (eur === null)) throw new Error('Give either pct or eur');
    const n = this.n, rows = this.selectCS(productCountries, productSectors), cols = this.selectFD(buyers, categories);
    const dy = new Float64Array(rows.length);
    if (pct !== null) {
      rows.forEach((i, r) => { let s = 0; for (const j of cols) s += this.Y[i + j * n]; dy[r] = s * pct / 100; });
    } else {
      const w = rows.map(i => { let s = 0; for (const j of cols) s += Math.max(this.Y[i + j * n], 0); return s; });
      const total = sum(w);
      if (!(total > 0)) throw new Error('There is no final demand in this selection');
      w.forEach((wi, r) => { dy[r] = eur * wi / total; });
    }
    const dx = new Float64Array(n);
    rows.forEach((i, r) => { if (dy[r]) this.addCol(this.L, i, dy[r], dx); });
    let direct = null, first = null;
    if (decompose) {
      this.requireA();
      direct = new Float64Array(n);
      rows.forEach((i, r) => { direct[i] = dy[r]; });
      first = this.aTimes(rows, dy);
    }
    const ind = induced ? this.inducedOutput(dx, spendRate) : null;
    return this.newImpact(dx, rows, sum(dy), 'final demand', direct, first, ind, induced ? spendRate : null);
  }

  // Output change everywhere when the output of S changes by dxS (S set from outside the model)
  outputChange(S, dxS) {
    const n = this.n, dx = new Float64Array(n), inS = new Uint8Array(n);
    S.forEach(i => { inS[i] = 1; });
    if (S.length === n) { S.forEach((s, a) => { dx[s] = dxS[a]; }); return dx; }
    if (S.length <= SMALL_SOURCE) {
      // dx_R = L[R,S] L[S,S]^-1 dxS
      this.requireL();
      const m = S.length, LSS = new Float64Array(m * m);
      for (let a = 0; a < m; a++) for (let b = 0; b < m; b++) LSS[a * m + b] = this.L[S[a] + S[b] * n];
      const z = solveDense(LSS, m, Float64Array.from(dxS));
      S.forEach((s, b) => { if (z[b]) this.addCol(this.L, s, z[b], dx); });
    } else {
      // Large S: x_R = A_RR x_R + A_RS dxS, solved by iteration (converges: A has column sums below 1)
      this.requireA();
      const b = this.aTimes(S, dxS);
      S.forEach(i => { b[i] = 0; });
      const { ptr, idx, val } = this.A;
      let x = Float64Array.from(b);
      for (let it = 0; it < 1000; it++) {
        const y = Float64Array.from(b);
        for (let j = 0; j < n; j++) {
          const xj = x[j];
          if (!xj || inS[j]) continue;
          for (let p = ptr[j]; p < ptr[j + 1]; p++) { const r = idx[p]; if (!inS[r]) y[r] += val[p] * xj; }
        }
        let diff = 0, scale = 0;
        for (let i = 0; i < n; i++) { diff = Math.max(diff, Math.abs(y[i] - x[i])); scale = Math.max(scale, Math.abs(y[i])); }
        x = y;
        if (diff <= 1e-11 * (1 + scale)) break;
      }
      dx.set(x);
    }
    S.forEach((s, a) => { dx[s] = dxS[a]; });
    return dx;
  }

  // Production shock: the output of the source country-industries changes by pct % (or eur in total,
  // spread by their output). Their suppliers follow.
  outputShock({ sourceCountries, sourceSectors = ['All sectors'], pct = null, eur = null, induced = false,
                spendRate = this.meta.defaults.spend_rate, decompose = true }) {
    if ((pct === null) === (eur === null)) throw new Error('Give either pct or eur');
    const S = this.selectCS(sourceCountries, sourceSectors).filter(i => this.x[i] > 0);
    if (!S.length) throw new Error('The source has no output');
    const totalX = sum(S.map(i => this.x[i]));
    const dxS = S.map(i => pct !== null ? this.x[i] * pct / 100 : eur * this.x[i] / totalX);
    const dx = this.outputChange(S, dxS);
    let direct = null, first = null;
    if (decompose) {
      this.requireA();
      direct = new Float64Array(this.n);
      S.forEach((s, a) => { direct[s] = dxS[a]; });
      first = this.aTimes(S, dxS);
      S.forEach(s => { first[s] = 0; });   // what they buy from each other is part of their (fixed) output
    }
    const ind = induced ? this.inducedOutput(dx, spendRate) : null;
    return this.newImpact(dx, S, sum(dxS), 'production', direct, first, ind, induced ? spendRate : null);
  }

  // ---- Household spending of extra wages (the "induced" effect) -------------------------------------
  // Consumer spending per EUR of extra wages, by country: one number for all, or {PL: 0.4, ...}
  spendRates(rate) {
    const out = new Float64Array(this.nc).fill(typeof rate === 'number' ? rate : this.meta.defaults.spend_rate);
    if (typeof rate === 'object' && rate !== null) {
      for (const [code, r] of Object.entries(rate)) {
        if (!this.countryIndex.has(code)) throw new Error(`Unknown country: ${code}`);
        out[this.countryIndex.get(code)] = r;
      }
    }
    if (out.some(r => !(r >= 0 && r <= 1))) throw new Error('Spending per EUR of wages must be between 0 and 1');
    return out;
  }
  // Extra output when households spend part of the extra wages, round after round:
  // dw = (I - W L H K)^-1 W dx1, extra output = L H K dw
  inducedOutput(dx1, rate) {
    const { n, nc } = this;
    const k = this.spendRates(rate).map((r, c) => r * this.basic[c]);
    const w1 = new Float64Array(nc);
    for (let i = 0; i < n; i++) w1[this.cOf(i)] += this.wage[i] * dx1[i];
    const M = new Float64Array(nc * nc);
    for (let r = 0; r < nc; r++) for (let c = 0; c < nc; c++) M[r * nc + c] = (r === c ? 1 : 0) - this.WLH[r + c * nc] * k[c];
    const dw = solveDense(M, nc, w1);
    const out = new Float64Array(n);
    for (let c = 0; c < nc; c++) { const a = k[c] * dw[c]; if (a) this.addCol(this.LH, c, a, out); }
    return out;
  }

  // ---- Totals of an impact ------------------------------------------------------------------------------
  // level: country, country_sector, sector, sector_group, country_sector_group, total.
  // Like summarise_impact() in R, sorted by dva (most negative first).
  summariseImpact(imp, level = 'country', countries = ['World'], minVA = 0) {
    const keep = new Set(this.expandCountries(countries).map(c => this.countryIndex.get(c)));
    const keyOf = {
      country: i => this.cOf(i), country_sector: i => i, sector: i => this.sOf(i),
      sector_group: i => this.sectorGroupOf[this.sOf(i)],
      country_sector_group: i => `${this.cOf(i)}|${this.sectorGroupOf[this.sOf(i)]}`, total: () => 'total'
    }[level];
    const groups = new Map(), countryVA = new Map();
    for (let i = 0; i < this.n; i++) {
      const c = this.cOf(i);
      if (!keep.has(c)) continue;
      countryVA.set(c, (countryVA.get(c) ?? 0) + this.va[i]);
      const key = keyOf(i);
      let g = groups.get(key);
      if (!g) {
        g = { i, va: 0, dva: 0, dva_excl_source: 0, dva_direct: 0, dva_first: 0, dva_further: 0,
              dva_induced: 0, dva_induced_excl_source: 0, in_source: true };
        if (imp.own) Object.assign(g, { dsales: 0, dva_own: 0, dva_other: 0 });   // a trade shock
        groups.set(key, g);
      }
      g.va += this.va[i]; g.dva += imp.dva[i];
      g.dva_direct += imp.direct[i]; g.dva_first += imp.first[i]; g.dva_further += imp.further[i];
      g.dva_induced += imp.dvaInduced[i];
      if (imp.own) { g.dsales += imp.dsales[i]; g.dva_own += imp.own[i]; g.dva_other += imp.other[i]; }
      if (!imp.inSource[i]) { g.dva_excl_source += imp.dva[i]; g.dva_induced_excl_source += imp.dvaInduced[i]; }
      g.in_source = g.in_source && imp.inSource[i] === 1;
    }
    const byCountry = ['country', 'country_sector', 'country_sector_group'].includes(level);
    const bySector = ['country_sector', 'sector'].includes(level);
    let out = [...groups.values()].map(g => {
      const row = { ...g, share_va: g.dva / g.va };
      if (byCountry) {
        const c = this.cOf(g.i);
        row.country = this.countryCodes[c]; row.country_name = this.countryNames[row.country];
        row.share_country_va = g.dva / countryVA.get(c);
      }
      if (bySector) { row.sector = this.sectorOf(g.i); row.sector_name = this.sectorNames[row.sector]; }
      if (level === 'sector_group' || level === 'country_sector_group') row.sector_group = this.sectorGroupOf[this.sOf(g.i)];
      if (level === 'country_sector') row.label = this.label(g.i);
      delete row.i;
      return row;
    });
    if (level !== 'country' && level !== 'total') out = out.filter(r => r.va >= minVA);
    return out.sort((a, b) => a.dva - b.dva);
  }

  // ---- 4. Dependence on a source (the "Exposure to a source" view) ---------------------------------------
  // Production: -100% production shock, sign turned. Final demand: value added from all final demand
  // for the source's products.
  dependenceImpact(type, srcCountries, srcSectors) {
    if (type === 'production') {
      const imp = this.outputShock({ sourceCountries: srcCountries, sourceSectors: srcSectors, pct: -100, decompose: false });
      for (let i = 0; i < this.n; i++) imp.dva[i] = -imp.dva[i];
      imp.size = -imp.size;
      return imp;
    }
    return this.fdShock({ productCountries: srcCountries, productSectors: srcSectors, pct: 100, decompose: false });
  }
  dependenceTable(imp, type, level, showCountries, minVA) {
    let s = this.summariseImpact(imp, level, showCountries, minVA);
    if (type === 'production') s = s.filter(r => !r.in_source).map(r => ({ ...r, dep: r.dva_excl_source }));
    else s = s.map(r => ({ ...r, dep: r.dva }));
    s.forEach(r => {
      r.share = r.dep / r.va;
      r.name = { country: r.country_name, country_sector: r.label, sector: r.sector_name, sector_group: r.sector_group }[level];
    });
    return s.sort((a, b) => b.share - a.share);
  }

  // Total final demand (all buyers, all categories) for each country-industry's products
  yTotal() {
    if (!this._yTotal) {
      const { n, nfd } = this, t = new Float64Array(n);
      for (let j = 0; j < nfd; j++) for (let i = 0; i < n; i++) t[i] += this.Y[i + j * n];
      this._yTotal = t;
    }
    return this._yTotal;
  }

  // What an origin depends on most (like sources_of() in R). One row per source with the share of the
  // origin's GVA that depends on it. sourceLevel: country_sector, country, or sector (final demand only).
  sourcesOf({ originCountries, originSectors = ['All sectors'], type = 'production', sourceLevel = 'country_sector',
              sourceCountries = ['World'], foreignOnly = true }) {
    this.requireL();
    const { n } = this, O = this.selectCS(originCountries, originSectors);
    const vaO = sum(O.map(i => this.va[i]));
    const own = new Set(O.map(i => this.countryOf(i)));
    const srcSet = new Set(this.expandCountries(sourceCountries));
    if (type === 'production' && sourceLevel === 'sector') {
      throw new Error('For production, choose single country-industries or whole countries as sources.');
    }
    let rows;
    if (type === 'production' && sourceLevel === 'country') {
      rows = this.countryCodes.filter(c => srcSet.has(c) && !own.has(c)).map(cc => {
        const S = this.selectCS([cc]).filter(i => this.x[i] > 0);
        const dx = this.outputChange(S, S.map(i => -this.x[i]));
        let dep = 0;
        for (const i of O) dep -= this.v[i] * dx[i];
        return { source: cc, source_name: this.countryNames[cc], va_dependent: dep };
      });
    } else {
      // GVA of the origin per EUR of final demand for each product: v_O L[O, ]
      const vOL = new Float64Array(n);
      for (let j = 0; j < n; j++) { let s = 0; const off = j * n; for (const i of O) s += this.v[i] * this.L[off + i]; vOL[j] = s; }
      const yT = this.yTotal(), inO = new Uint8Array(n);
      O.forEach(i => { inO[i] = 1; });
      const agg = new Map();
      for (let j = 0; j < n; j++) {
        const c = this.countryOf(j);
        if (!srcSet.has(c) || (foreignOnly && own.has(c))) continue;
        if (type === 'production' && inO[j]) continue;          // an origin does not depend on itself
        const contrib = type === 'production' ? vOL[j] * this.x[j] / this.L[j + j * n] : vOL[j] * yT[j];
        if (!Number.isFinite(contrib)) continue;
        const key = sourceLevel === 'country_sector' ? j : sourceLevel === 'country' ? c : this.sectorOf(j);
        const name = sourceLevel === 'country_sector' ? this.label(j) : sourceLevel === 'country' ? this.countryNames[c] : this.sectorNames[this.sectorOf(j)];
        const source = sourceLevel === 'country_sector' ? `${c}_${this.sectorOf(j)}` : key;
        const a = agg.get(key) ?? { source, source_name: name, va_dependent: 0 };
        a.va_dependent += contrib;
        agg.set(key, a);
      }
      rows = [...agg.values()];
    }
    rows.forEach(r => { r.share = r.va_dependent / vaO; r.origin_va = vaO; });
    return rows.sort((a, b) => b.share - a.share);
  }

  // The most exposed (origin, source) pairs of country-industries (like top_pairs() in R).
  // Share = part of the origin's GVA that depends on the source (production or final demand for its products).
  topPairs({ originCountries, originSectors = ['All sectors'], sourceCountries = ['World'], sourceSectors = ['All sectors'],
             type = 'production', foreignOnly = true, minVA = 0, nTop = 50 }) {
    this.requireL();
    const { n } = this;
    const O = this.selectCS(originCountries, originSectors).filter(i => this.va[i] >= minVA && this.x[i] > 0);
    const S = this.selectCS(sourceCountries, sourceSectors);
    const yT = this.yTotal();
    const w = S.map(s => type === 'production' ? this.x[s] / this.L[s + s * n] : yT[s]);
    let best = [], cutoff = -Infinity;
    S.forEach((s, b) => {
      const cs = this.cOf(s), off = s * n, ws = w[b];
      if (!Number.isFinite(ws) || ws === 0) return;
      for (const r of O) {
        if (type === 'production' && r === s) continue;
        if (foreignOnly && this.cOf(r) === cs) continue;
        const d = this.L[off + r] * ws / this.x[r];
        if (d > cutoff) {
          best.push([d, r, s]);
          if (best.length > 4 * nTop + 100) { best.sort((p, q) => q[0] - p[0]); best.length = nTop; cutoff = best[nTop - 1][0]; }
        }
      }
    });
    best.sort((p, q) => q[0] - p[0]);
    return best.slice(0, nTop).map(([d, r, s]) => ({
      origin: this.label(r), origin_country: this.countryOf(r), origin_sector: this.sectorOf(r),
      source: this.label(s), source_country: this.countryOf(s), source_sector: this.sectorOf(s),
      share: d, va_dependent: d * this.va[r], origin_va: this.va[r]
    }));
  }

  // ---- 5. The home economy --------------------------------------------------------------------------------
  domesticShock({ country, type = 'final_demand', categories = ['All'], sectors = ['All sectors'], productsFrom = 'all',
                  pct = null, eur = null, induced = false, spendRate = this.meta.defaults.spend_rate, decompose = true }) {
    if (!this.countryIndex.has(country)) throw new Error('Give one country code, e.g. "PL"');
    if (type === 'final_demand') {
      return this.fdShock({ buyers: [country], categories, productCountries: productsFrom === 'home' ? [country] : ['World'],
                            productSectors: sectors, pct, eur, induced, spendRate, decompose });
    }
    return this.outputShock({ sourceCountries: [country], sourceSectors: sectors, pct, eur, induced, spendRate, decompose });
  }

  // Headline numbers of a shock seen from one country (like home_vs_abroad() in R)
  homeVsAbroad(imp, home) {
    const c = this.countryIndex.get(home);
    const h = { shock: imp.size, dva_home: 0, dva_home_other: 0, dva_home_direct: 0, dva_home_first: 0,
                dva_home_further: 0, dva_home_induced: 0, dva_abroad: 0, home_va: 0 };
    for (let i = 0; i < this.n; i++) {
      if (this.cOf(i) === c) {
        h.dva_home += imp.dva[i]; h.home_va += this.va[i];
        if (!imp.inSource[i]) h.dva_home_other += imp.dva[i];
        h.dva_home_direct += imp.direct[i]; h.dva_home_first += imp.first[i];
        h.dva_home_further += imp.further[i]; h.dva_home_induced += imp.dvaInduced[i];
      } else {
        h.dva_abroad += imp.dva[i];
      }
    }
    h.share_home_va = h.dva_home / h.home_va;
    h.home_per_eur = h.dva_home / h.shock;
    h.home_per_eur_type1 = (h.dva_home - h.dva_home_induced) / h.shock;
    h.abroad_per_eur = h.dva_abroad / h.shock;
    h.leakage = h.dva_abroad / (h.dva_home + h.dva_abroad);
    return h;
  }

  // Where EUR 1 of the country's own final demand ends up, by category (like domestic_content() in R)
  domesticContent(country, induced = false, spendRate = this.meta.defaults.spend_rate) {
    const { n, nk } = this, c = this.countryIndex.get(country);
    const home = new Uint8Array(n);
    this.homeRows(country).forEach(i => { home[i] = 1; });
    const rows = [], dx1s = [];
    const all = { code: 'ALL', category: 'All final demand', final_demand: 0, va_home: 0, va_abroad: 0 };
    for (let k = 0; k < nk; k++) {
      const j = c * nk + k, code = this.catCodes[k];
      const r = { code, category: this.catLabels[code], final_demand: 0, va_home: 0, va_abroad: 0 };
      for (let i = 0; i < n; i++) {
        r.final_demand += this.Y[i + j * n];
        if (home[i]) r.va_home += this.VAD[i + j * n]; else r.va_abroad += this.VAD[i + j * n];
      }
      ['final_demand', 'va_home', 'va_abroad'].forEach(f => { all[f] += r[f]; });
      rows.push(r);
      if (induced) {
        this.requireL();
        const dx1 = new Float64Array(n);
        for (let i = 0; i < n; i++) { const y = this.Y[i + j * n]; if (y) this.addCol(this.L, i, y, dx1); }
        dx1s.push(dx1);
      }
    }
    rows.push(all);
    if (induced) dx1s.push(dx1s.reduce((acc, d) => { for (let i = 0; i < n; i++) acc[i] += d[i]; return acc; }, new Float64Array(n)));
    rows.forEach((r, idx) => {
      r.home_per_eur = r.va_home / r.final_demand;
      r.abroad_per_eur = r.va_abroad / r.final_demand;
      r.other_per_eur = 1 - (r.va_home + r.va_abroad) / r.final_demand;
      if (induced) {
        const ind = this.inducedOutput(dx1s[idx], spendRate);
        r.va_home_hh = 0;
        for (let i = 0; i < n; i++) if (home[i]) r.va_home_hh += this.v[i] * ind[i];
        r.home_hh_per_eur = r.va_home_hh / r.final_demand;
      }
    });
    return rows;
  }

  // Home multipliers by industry: per EUR 1 of final demand for a home industry's products
  // (like domestic_multipliers() in R)
  domesticMultipliers(country, induced = false, spendRate = this.meta.defaults.spend_rate) {
    this.requireL();
    const { n, nfd } = this, S = this.homeRows(country), home = new Uint8Array(n);
    S.forEach(i => { home[i] = 1; });
    return S.map(s => {
      const off = s * n;
      const r = { sector: this.sectorOf(s), sector_name: this.sectorNames[this.sectorOf(s)],
                  sector_group: this.sectorGroupOf[this.sOf(s)], va: this.va[s], final_demand: 0,
                  output_multiplier: 0, va_own: this.v[s] * this.L[off + s], va_home: 0, va_abroad: 0 };
      for (let j = 0; j < nfd; j++) r.final_demand += this.Y[s + j * n];
      for (let i = 0; i < n; i++) {
        const g = this.v[i] * this.L[off + i];
        if (home[i]) { r.va_home += g; r.output_multiplier += this.L[off + i]; } else r.va_abroad += g;
      }
      r.va_other_home = r.va_home - r.va_own;
      r.other = 1 - r.va_home - r.va_abroad;
      if (induced) {
        const ind = this.inducedOutput(this.L.subarray(off, off + n), spendRate);
        r.va_home_hh = 0;
        for (let i = 0; i < n; i++) if (home[i]) r.va_home_hh += this.v[i] * ind[i];
      }
      if (!(this.x[s] > 0)) ['output_multiplier', 'va_own', 'va_home', 'va_abroad', 'va_other_home', 'other', 'va_home_hh']
        .forEach(f => { r[f] = NaN; });
      return r;
    });
  }

  // Share of each home industry's (or group's) GVA that depends on each category of home final demand
  // and on foreign final demand (like home_demand_exposure() in R). Long format.
  homeDemandExposure(country, level = 'sector') {
    const { n, nk, nfd } = this, c = this.countryIndex.get(country);
    const groups = new Map();
    for (const i of this.homeRows(country)) {
      const key = level === 'sector' ? this.sectorOf(i) : this.sectorGroupOf[this.sOf(i)];
      let g = groups.get(key);
      if (!g) { g = { va: 0, parts: new Float64Array(nk + 1) }; groups.set(key, g); }
      g.va += this.va[i];
      for (let j = 0; j < nfd; j++) {
        const b = Math.floor(j / nk);
        g.parts[b === c ? j % nk : nk] += this.VAD[i + j * n];
      }
    }
    const out = [];
    const codes = [...this.catCodes, 'FOREIGN'];
    for (const [origin, g] of groups) {
      codes.forEach((code, p) => out.push({
        origin, origin_name: level === 'sector' ? this.sectorNames[origin] : origin, va: g.va, code,
        demand: code === 'FOREIGN' ? 'Foreign final demand' : this.catLabels[code],
        va_dependent: g.parts[p], share: g.parts[p] / g.va
      }));
    }
    return out;
  }

  // ---- 6. Price shocks (cost push) ------------------------------------------------------------------------
  // Like price_shock() in R. The price model p = p A + v: the output price of the source country-industries
  // S rises by pct % (set from outside); every buyer passes its extra input costs on in full, along the whole
  // supply chain. dp_R = dp_S L[S,S]^-1 L[S,R]. Per country-industry:
  //   dp        price change of its output (in S: the shock itself)
  //   direct    extra cost of what it buys directly from S, per EUR of output (the lower end)
  //   unit      extra input cost per EUR of output (outside S it equals dp); indirect = unit - direct
  priceShock({ sourceCountries, sourceSectors = ['All sectors'], pct = 10 }) {
    if (!Number.isFinite(pct) || pct === 0) throw new Error('Give a price change other than 0.');
    this.requireA();
    const n = this.n, d = pct / 100;
    const S = this.selectCS(sourceCountries, sourceSectors).filter(i => this.x[i] > 0);
    if (!S.length) throw new Error('The source has no output');
    const inS = new Uint8Array(n);
    S.forEach(i => { inS[i] = 1; });
    const dp = new Float64Array(n), { ptr, idx, val } = this.A;
    if (S.length <= SMALL_SOURCE) {
      // w = (L[S,S]')^-1 dS, then dp_R = L[S,R]' w
      this.requireL();
      const m = S.length, M = new Float64Array(m * m);
      for (let a = 0; a < m; a++) for (let b = 0; b < m; b++) M[a * m + b] = this.L[S[b] + S[a] * n];
      const w = solveDense(M, m, new Float64Array(m).fill(d));
      for (let j = 0; j < n; j++) {
        if (inS[j]) continue;
        const off = j * n;
        let s = 0;
        for (let b = 0; b < m; b++) s += this.L[off + S[b]] * w[b];
        dp[j] = s;
      }
    } else {
      // Large S: p_R = p_S A[S,R] + p_R A[R,R], solved by iteration (converges: A has column sums below 1)
      const b0 = new Float64Array(n);
      for (let j = 0; j < n; j++) {
        if (inS[j]) continue;
        for (let p = ptr[j]; p < ptr[j + 1]; p++) if (inS[idx[p]]) b0[j] += val[p] * d;
      }
      let x = Float64Array.from(b0);
      for (let it = 0; it < 1000; it++) {
        const y = Float64Array.from(b0);
        let diff = 0, scale = 0;
        for (let j = 0; j < n; j++) {
          if (inS[j]) continue;
          for (let p = ptr[j]; p < ptr[j + 1]; p++) { const k = idx[p]; if (!inS[k]) y[j] += val[p] * x[k]; }
          diff = Math.max(diff, Math.abs(y[j] - x[j])); scale = Math.max(scale, Math.abs(y[j]));
        }
        x = y;
        if (diff <= 1e-11 * (1 + scale)) break;
      }
      dp.set(x);
    }
    S.forEach(i => { dp[i] = d; });
    // Extra input cost per EUR of output, and the part bought directly from S
    const unit = new Float64Array(n), direct = new Float64Array(n);
    for (let j = 0; j < n; j++) {
      for (let p = ptr[j]; p < ptr[j + 1]; p++) {
        const k = idx[p];
        unit[j] += val[p] * dp[k];
        if (inS[k]) direct[j] += val[p] * d;
      }
    }
    return { what: 'price', size: pct, nSource: S.length, sourceOutput: sum(S.map(i => this.x[i])),
             dp, unit, direct, inSource: inS };
  }

  // Totals of a price shock (like summarise_price() in R). The producers whose price is set are left out;
  // prices are averaged with output weights. Sorted by price change, largest first.
  summarisePrice(pr, level = 'country', countries = ['World'], minVA = 0) {
    const keep = new Set(this.expandCountries(countries).map(c => this.countryIndex.get(c)));
    const keyOf = {
      country: i => this.cOf(i), country_sector: i => i, sector: i => this.sOf(i),
      sector_group: i => this.sectorGroupOf[this.sOf(i)],
      country_sector_group: i => `${this.cOf(i)}|${this.sectorGroupOf[this.sOf(i)]}`, total: () => 'total'
    }[level];
    const groups = new Map();
    for (let i = 0; i < this.n; i++) {
      if (!keep.has(this.cOf(i)) || pr.inSource[i]) continue;
      const key = keyOf(i), x = Math.max(this.x[i], 0);
      let g = groups.get(key);
      if (!g) { g = { i, x: 0, va: 0, cost: 0, cost_direct: 0 }; groups.set(key, g); }
      g.x += x; g.va += this.va[i]; g.cost += pr.unit[i] * x; g.cost_direct += pr.direct[i] * x;
    }
    const byCountry = ['country', 'country_sector', 'country_sector_group'].includes(level);
    const bySector = ['country_sector', 'sector'].includes(level);
    let out = [...groups.values()].map(g => {
      const row = { x: g.x, va: g.va, cost: g.cost, cost_direct: g.cost_direct,
                    dp: g.x > 0 ? g.cost / g.x : NaN, dp_direct: g.x > 0 ? g.cost_direct / g.x : NaN };
      row.dp_indirect = row.dp - row.dp_direct;
      row.cost_share_va = g.cost / g.va;
      if (byCountry) { row.country = this.countryOf(g.i); row.country_name = this.countryNames[row.country]; }
      if (bySector) { row.sector = this.sectorOf(g.i); row.sector_name = this.sectorNames[row.sector]; }
      if (level === 'sector_group' || level === 'country_sector_group') row.sector_group = this.sectorGroupOf[this.sOf(g.i)];
      if (level === 'country_sector') row.label = this.label(g.i);
      return row;
    });
    if (level !== 'country' && level !== 'total') out = out.filter(r => r.va >= minVA);
    return out.sort((a, b) => (Number.isFinite(b.dp) ? b.dp : -Infinity) - (Number.isFinite(a.dp) ? a.dp : -Infinity));
  }

  // Price change of what final buyers buy, by buyer country and category, at basic prices (like
  // basket_prices() in R): dp, of which dp_source (products of the shocked producers bought directly), dp_other.
  basketPrices(pr, buyers = ['World'], categories = ['P3_S14', 'P3_S13', 'P51G']) {
    const n = this.n;
    return this.selectFD(buyers, categories).map(j => {
      let tot = 0, all = 0, src = 0;
      for (let i = 0; i < n; i++) {
        const y = Math.max(this.Y[i + j * n], 0);
        if (!y) continue;
        tot += y; all += pr.dp[i] * y;
        if (pr.inSource[i]) src += pr.dp[i] * y;
      }
      const buyer = this.countryCodes[Math.floor(j / this.nk)], code = this.catCodes[j % this.nk];
      return { buyer, buyer_name: this.countryNames[buyer], code, category: this.catLabels[code], final_demand: tot,
               dp: all / tot, dp_source: src / tot, dp_other: (all - src) / tot };
    });
  }

  // ---- 7. Trade-flow shocks (like trade_shock() in R) -------------------------------------------------------
  // Gross exports of each country-industry to each importer country (sales within a country count as 0).
  // flows: all, final (to final buyers), intermediate (to industries). Returns {B: importer indexes, F} with
  // F[i + col * n] = exports of i to importer B[col].
  exportsTo(importers, flows = 'all') {
    const { n, nk } = this;
    const B = this.expandCountries(importers).map(c => this.countryIndex.get(c));
    const F = new Float64Array(n * B.length);
    B.forEach((b, col) => {
      for (let i = 0; i < n; i++) {
        if (this.cOf(i) === b) continue;
        const all = this.sales[i + b * n];
        let fin = 0;
        if (flows !== 'all') for (let k = 0; k < nk; k++) fin += this.Y[i + (b * nk + k) * n];
        F[i + col * n] = flows === 'all' ? all : flows === 'final' ? fin : all - fin;
      }
    });
    return { B, F };
  }

  // A change in what the exporters sell to the importers. replace: none, importer (producers of the same
  // industry in the importing country make up replaceShare of it), others (producers of the same industry in
  // other countries, in proportion to what they sell to the importer now). Every producer's change in sales
  // works like a change in final demand for its products. The impact also has dsales (the change in sales),
  // own (GVA change from sales changes of producers in the same country) and other (from producers abroad).
  tradeShock({ exporters, exporterSectors = ['All sectors'], importers, flows = 'all', pct = null, eur = null,
               replace = 'none', replaceShare = 1, induced = false, spendRate = this.meta.defaults.spend_rate,
               decompose = true }) {
    this.requireL();
    if ((pct === null) === (eur === null)) throw new Error('Give either pct or eur');
    if (!(replaceShare >= 0 && replaceShare <= 1)) throw new Error('The share made up must be between 0 and 1');
    const { n, ns, nc } = this;
    const O = this.selectCS(exporters, exporterSectors), m = O.length;
    const { B, F } = this.exportsTo(importers, flows), nb = B.length;
    let total = 0, totalPos = 0, any = false;
    for (const i of O) for (let b = 0; b < nb; b++) { const f = F[i + b * n]; total += f; totalPos += Math.max(f, 0); if (f !== 0) any = true; }
    if (!any || (eur !== null && !(totalPos > 0))) throw new Error('The exporters sell nothing to the importers');
    const dS = new Float64Array(m * nb);    // change in the exports of O[r] to B[b]: dS[r + b * m]
    O.forEach((i, r) => {
      for (let b = 0; b < nb; b++) {
        const f = F[i + b * n];
        dS[r + b * m] = pct !== null ? f * pct / 100 : eur * Math.max(f, 0) / totalPos;
      }
    });
    const dy = new Float64Array(n);
    O.forEach((i, r) => { for (let b = 0; b < nb; b++) dy[i] += dS[r + b * m]; });
    let madeUp = 0;
    if (replace !== 'none' && replaceShare > 0) {
      const excluded = new Set(this.expandCountries(exporters).map(c => this.countryIndex.get(c)));
      for (let b = 0; b < nb; b++) {
        const need = new Float64Array(ns);
        O.forEach((i, r) => { need[this.sOf(i)] -= replaceShare * dS[r + b * m]; });
        for (let s = 0; s < ns; s++) {
          if (!need[s]) continue;
          if (replace === 'importer') { dy[B[b] * ns + s] += need[s]; madeUp += need[s]; continue; }
          let tot = 0;
          for (let c = 0; c < nc; c++) if (!excluded.has(c) && c !== B[b]) tot += Math.max(F[c * ns + s + b * n], 0);
          if (!(tot > 0)) continue;   // nobody else sells this to the importer
          for (let c = 0; c < nc; c++) {
            if (excluded.has(c) || c === B[b]) continue;
            const w = Math.max(F[c * ns + s + b * n], 0);
            if (w) dy[c * ns + s] += need[s] * w / tot;
          }
          madeUp += need[s];
        }
      }
    }
    const dx = new Float64Array(n), own = new Float64Array(n), cols = [], vals = [];
    for (let j = 0; j < n; j++) {
      const d = dy[j];
      if (!d) continue;
      cols.push(j); vals.push(d);
      this.addCol(this.L, j, d, dx);
      const off = j * n, c0 = this.cOf(j) * ns;
      for (let i = c0; i < c0 + ns; i++) own[i] += this.L[off + i] * d;
    }
    let direct = null, first = null;
    if (decompose) { this.requireA(); direct = Float64Array.from(dy); first = this.aTimes(cols, vals); }
    const ind = induced ? this.inducedOutput(dx, spendRate) : null;
    const imp = this.newImpact(dx, O, sum(dS), 'trade', direct, first, ind, induced ? spendRate : null);
    imp.dsales = dy;
    imp.own = new Float64Array(n); imp.other = new Float64Array(n);
    for (let i = 0; i < n; i++) { imp.own[i] = this.v[i] * own[i]; imp.other[i] = this.v[i] * (dx[i] - own[i]); }
    Object.assign(imp, { exports: total, madeUp, flows, replace, replaceShare });
    return imp;
  }

  // ---- 8. Value added in exports (like export_content(), export_sources(), va_in_others_exports() in R) -----
  // Gross exports of each country-industry to the importers, all flows (negative ones, stock drawdowns, count as 0)
  exportVector(importers) {
    const n = this.n, e = new Float64Array(n);
    for (const code of this.expandCountries(importers)) {
      const b = this.countryIndex.get(code);
      for (let i = 0; i < n; i++) if (this.cOf(i) !== b) e[i] += this.sales[i + b * n];
    }
    for (let i = 0; i < n; i++) if (e[i] < 0) e[i] = 0;
    return e;
  }
  exportRows(exporters, exporterSectors, importers) {
    this.requireL();
    const e = this.exportVector(importers);
    const R = this.selectCS(exporters, exporterSectors).filter(i => e[i] > 0);
    if (!R.length) throw new Error('These exporters sell nothing to the importers');
    return { R, e };
  }
  // Key and name of a country-industry grouped by total, country, sector or country_sector
  groupKey(i, by) {
    return by === 'total' ? 'Total' : by === 'country' ? this.countryOf(i) : by === 'sector' ? this.sectorOf(i)
      : `${this.countryOf(i)}_${this.sectorOf(i)}`;
  }
  groupName(key, by) {
    if (by === 'country') return this.countryNames[key];
    if (by === 'sector') return this.sectorNames[key];
    if (by === 'country_sector') { const k = key.indexOf('_'); return `${this.shortCode(key.slice(0, k))} ${this.sectorNames[key.slice(k + 1)]}`; }
    return key;
  }
  // One row per exporter: gross exports, home GVA, foreign GVA and other in them, and their shares
  exportContent({ exporters, exporterSectors = ['All sectors'], importers = ['World'], by = 'country' }) {
    const { R, e } = this.exportRows(exporters, exporterSectors, importers);
    const { n, ns } = this, groups = new Map();
    for (const r of R) {
      const key = this.groupKey(r, by), off = r * n, c0 = this.cOf(r) * ns;
      let all = 0, home = 0;
      for (let i = 0; i < n; i++) all += this.v[i] * this.L[off + i];
      for (let i = c0; i < c0 + ns; i++) home += this.v[i] * this.L[off + i];
      let g = groups.get(key);
      if (!g) { g = { exporter: key, exporter_name: this.groupName(key, by), exports: 0, va_home: 0, va_foreign: 0 }; groups.set(key, g); }
      g.exports += e[r]; g.va_home += home * e[r]; g.va_foreign += (all - home) * e[r];
    }
    return [...groups.values()].map(g => {
      const other = g.exports - g.va_home - g.va_foreign;
      return { ...g, other, home_share: g.va_home / g.exports, foreign_share: g.va_foreign / g.exports, other_share: other / g.exports };
    }).sort((a, b) => b.foreign_share - a.foreign_share);
  }
  // Where the value added in the exports comes from: rows (exporter, source, home) with va and its share of the
  // exporter's exports. by: total, country, sector; sourceLevel: country, country_sector, sector.
  exportSources({ exporters, exporterSectors = ['All sectors'], importers = ['World'], by = 'total', sourceLevel = 'country',
                  foreignOnly = true }) {
    const { R, e } = this.exportRows(exporters, exporterSectors, importers);
    const { n, ns, nc } = this;
    const nsrc = sourceLevel === 'country' ? nc : sourceLevel === 'sector' ? ns : n;
    const srcOf = Int32Array.from({ length: n }, (_, i) => sourceLevel === 'country' ? this.cOf(i) : sourceLevel === 'sector' ? this.sOf(i) : i);
    const cOfAll = Int32Array.from({ length: n }, (_, i) => this.cOf(i));
    const groups = new Map();
    for (const r of R) {
      const key = this.groupKey(r, by);
      let g = groups.get(key);
      if (!g) { g = { exports: 0, foreign: new Float64Array(nsrc), home: new Float64Array(nsrc) }; groups.set(key, g); }
      const off = r * n, er = e[r], cr = cOfAll[r];
      g.exports += er;
      for (let i = 0; i < n; i++) {
        const k = this.v[i] * this.L[off + i] * er;
        if (!k) continue;
        if (cOfAll[i] === cr) g.home[srcOf[i]] += k; else g.foreign[srcOf[i]] += k;
      }
    }
    const srcCode = s => sourceLevel === 'country' ? this.countryCodes[s] : sourceLevel === 'sector' ? this.sectorCodes[s] : `${this.countryOf(s)}_${this.sectorOf(s)}`;
    const srcName = s => sourceLevel === 'country' ? this.countryNames[this.countryCodes[s]] : sourceLevel === 'sector' ? this.sectorNames[this.sectorCodes[s]] : this.label(s);
    const out = [];
    for (const [key, g] of groups) {
      const parts = foreignOnly ? [[false, g.foreign]] : [[false, g.foreign], [true, g.home]];
      for (const [home, a] of parts) {
        for (let s = 0; s < nsrc; s++) {
          if (!a[s]) continue;
          const row = { exporter: key, exporter_name: this.groupName(key, by), source: srcCode(s), source_name: srcName(s), home,
                        va: a[s], share: a[s] / g.exports, exports: g.exports };
          if (sourceLevel !== 'sector') row.source_country = sourceLevel === 'country' ? row.source : this.countryOf(s);
          out.push(row);
        }
      }
    }
    return out.sort((a, b) => b.va - a.va);
  }
  // GVA of an origin in other countries' gross exports (to the importers), by exporting country
  vaInOthersExports({ originCountries, originSectors = ['All sectors'], importers = ['World'] }) {
    this.requireL();
    const { n, nc } = this, e = this.exportVector(importers), O = this.selectCS(originCountries, originSectors);
    const M = new Float64Array(O.length * nc);   // GVA of O[r] in the exports of country d: M[r * nc + d]
    for (let j = 0; j < n; j++) {
      const ej = e[j];
      if (!ej) continue;
      const d = this.cOf(j), off = j * n;
      for (let r = 0; r < O.length; r++) { const i = O[r]; M[r * nc + d] += this.v[i] * this.L[off + i] * ej; }
    }
    const exports = new Float64Array(nc), byD = new Float64Array(nc);
    for (let j = 0; j < n; j++) exports[this.cOf(j)] += e[j];
    let own = 0, vaO = 0;
    O.forEach((i, r) => {
      vaO += this.va[i];
      const ci = this.cOf(i);
      for (let d = 0; d < nc; d++) { if (d === ci) own += M[r * nc + d]; else byD[d] += M[r * nc + d]; }
    });
    const rows = this.countryCodes.map((code, d) => ({ exporter: code, exporter_name: this.countryNames[code], exports: exports[d], va: byD[d],
                                                       share_origin_va: byD[d] / vaO, share_of_exports: byD[d] / exports[d] }))
      .filter(r => r.va !== 0).sort((a, b) => b.va - a.va);
    return { by_exporter: rows, origin_va: vaO, in_own_exports: own };
  }

  // ---- 9. Profile of country-industries (like profile_figures() ... profile_suppliers() in R) -------------------
  profileRows(countries, sectors) {
    const P = this.selectCS(countries, sectors).filter(i => this.x[i] > 0);
    if (!P.length) throw new Error('The selection has no output');
    return P;
  }
  // Key figures: rows {part, part_name, figure, label, type, value}; parts: Total and, with several countries, each one
  profileFigures(countries, sectors = ['All sectors']) {
    this.requireL(); this.requireA();
    const { n, ns, nc, nk, nfd } = this, P = this.profileRows(countries, sectors);
    const { ptr, idx, val } = this.A;
    const energyRows = this.selectCS(['World'], this.meta.energy_sectors), isEnergy = new Uint8Array(n);
    energyRows.forEach(i => { isEnergy[i] = 1; });
    const energy = this.priceShock({ sourceCountries: ['World'], sourceSectors: this.meta.energy_sectors, pct: 100 });
    const gov = this.catCodes.indexOf('P3_S13');
    const xPos = Float64Array.from(this.x, v => Math.max(v, 0));
    const rows = P.map(j => {
      const c = this.cOf(j), x = this.x[j], off = j * n, c0 = c * ns;
      let imported = 0, energyDirect = 0;
      for (let p = ptr[j]; p < ptr[j + 1]; p++) {
        const i = idx[p];
        if (this.cOf(i) !== c) imported += val[p];
        if (isEnergy[i]) energyDirect += val[p];
      }
      let vlAll = 0, vlHome = 0, lHome = 0;
      for (let i = 0; i < n; i++) vlAll += this.v[i] * this.L[off + i];
      for (let i = c0; i < c0 + ns; i++) { vlHome += this.v[i] * this.L[off + i]; lHome += this.L[off + i]; }
      let exports = 0, exportsFinal = 0;
      for (let b = 0; b < nc; b++) {
        if (b === c) continue;
        exports += this.sales[j + b * n];
        for (let k = 0; k < nk; k++) exportsFinal += this.Y[j + (b * nk + k) * n];
      }
      let vaAbroad = 0, vaGov = 0;
      for (let q = 0; q < nfd; q++) {
        const vad = this.VAD[j + q * n];
        if (Math.floor(q / nk) !== c) vaAbroad += vad;
        if (q % nk === gov) vaGov += vad;
      }
      let up = 0;
      for (let k = 0; k < n; k++) up += this.L[j + k * n] * xPos[k];
      return { c, x, va: this.va[j], wage: this.wage[j] * x, gos: this.gos[j] * x, exports, exportsFinal,
               imported: imported * x, vaHomeChain: vlHome * x, vaForeignChain: (vlAll - vlHome) * x, outputMult: lHome * x,
               vaAbroad, vaGov, energyDirect: energyDirect * x, energyTotal: energy.unit[j] * x, up };
    });
    const countryVA = new Float64Array(nc);
    for (let i = 0; i < n; i++) countryVA[this.cOf(i)] += this.va[i];
    const cs = [...new Set(rows.map(r => r.c))];
    const parts = [['Total', rows], ...(cs.length > 1 ? cs.map(c => [this.countryCodes[c], rows.filter(r => r.c === c)]) : [])];
    const out = [];
    for (const [part, rs] of parts) {
      const t = k => sum(rs.map(r => r[k]));
      const X = t('x'), V = t('va'), G = t('gos'), E = t('energyTotal');
      const vals = {
        output: X, va: V, va_share_country: V / sum([...new Set(rs.map(r => r.c))].map(c => countryVA[c])),
        va_per_output: V / X, wage_share: t('wage') / V, gos_output: G / X,
        exports: t('exports') / X, exports_final: t('exportsFinal') / X, imported_inputs: t('imported') / X,
        home_va_chain: t('vaHomeChain') / X, foreign_va_chain: t('vaForeignChain') / X, output_mult: t('outputMult') / X,
        va_abroad: t('vaAbroad') / V, va_gov: t('vaGov') / V, energy_direct: t('energyDirect') / X, energy_total: E / X,
        energy10_va: 0.1 * E / V, energy10_gos: G > 0 ? 0.1 * E / G : NaN, upstream: t('up') / X
      };
      for (const [figure, label, type] of PROFILE_LABELS) {
        out.push({ part, part_name: part === 'Total' ? 'Total' : this.countryNames[part], figure, label, type, value: vals[figure] });
      }
    }
    return out;
  }
  // What each EUR of output pays for: one row per country-industry (largest first), the total on top when there are several
  profileCosts(countries, sectors = ['All sectors']) {
    this.requireA();
    const P = this.profileRows(countries, sectors), { ptr, idx, val } = this.A;
    let rows = P.map(j => {
      const c = this.cOf(j), x = this.x[j];
      let home = 0, imported = 0;
      for (let p = ptr[j]; p < ptr[j + 1]; p++) { if (this.cOf(idx[p]) === c) home += val[p]; else imported += val[p]; }
      const comp = this.wage[j] * x, gos = this.gos[j] * x, va = this.va[j];
      return { part: this.label(j), country: this.countryOf(j), sector: this.sectorOf(j), x, va, inputs_home: home * x,
               inputs_imported: imported * x, taxes_other: x - (home + imported) * x - va, compensation: comp,
               other_taxes: va - comp - gos, gos };
    }).sort((a, b) => b.x - a.x);
    const parts = ['inputs_home', 'inputs_imported', 'taxes_other', 'compensation', 'other_taxes', 'gos'];
    if (rows.length > 1) {
      const tot = { part: 'Total', country: null, sector: null };
      for (const k of ['x', 'va', ...parts]) tot[k] = sum(rows.map(r => r[k]));
      rows = [tot, ...rows];
    }
    return rows.map(r => ({ ...r, ...Object.fromEntries(parts.map(k => [k, r[k] / r.x])) }));
  }
  // Who buys the output, as a share of it. level: buyer (single buyers) or country (home first)
  profileBuyers(countries, sectors = ['All sectors'], level = 'buyer') {
    const { n, nc, nk, nfd } = this, P = this.profileRows(countries, sectors);
    const X = sum(P.map(i => this.x[i]));
    let out;
    if (level === 'buyer') {
      this.requireA();
      const inP = new Uint8Array(n), { ptr, idx, val } = this.A;
      P.forEach(i => { inP[i] = 1; });
      out = [];
      for (let k = 0; k < n; k++) {
        let z = 0;
        for (let p = ptr[k]; p < ptr[k + 1]; p++) if (inP[idx[p]]) z += val[p];
        z *= this.x[k];
        if (z) out.push({ buyer: this.label(k), country: this.countryOf(k), type: 'Industries (inputs)', sales: z });
      }
      for (let q = 0; q < nfd; q++) {
        let y = 0;
        for (const i of P) y += this.Y[i + q * n];
        const b = this.countryCodes[Math.floor(q / nk)];
        if (y) out.push({ buyer: `${this.shortCode(b)} ${this.catLabels[this.catCodes[q % nk]]}`, country: b, type: 'Final buyers', sales: y });
      }
      out.sort((a, b) => b.sales - a.sales);
    } else {
      const ind = new Float64Array(nc), fin = new Float64Array(nc);
      let homeInd = 0, homeFin = 0;
      for (const i of P) {
        const c = this.cOf(i);
        for (let b = 0; b < nc; b++) {
          let f = 0;
          for (let k = 0; k < nk; k++) f += this.Y[i + (b * nk + k) * n];
          const s = this.sales[i + b * n] - f;
          if (b === c) { homeInd += s; homeFin += f; } else { ind[b] += s; fin[b] += f; }
        }
      }
      const cs = new Set(P.map(i => this.cOf(i)));
      const homeName = cs.size === 1 ? `${this.countryNames[this.countryOf(P[0])]} (home)` : 'Home (each one’s own country)';
      const rest = this.countryCodes.map((code, b) => ({ buyer: this.countryNames[code], country: code, industries: ind[b], final: fin[b], sales: ind[b] + fin[b] }))
        .filter(r => r.sales !== 0).sort((a, b) => b.sales - a.sales);
      out = [{ buyer: homeName, country: null, industries: homeInd, final: homeFin, sales: homeInd + homeFin }, ...rest];
    }
    return out.map(r => ({ ...r, share: r.sales / X }));
  }
  // Where the inputs come from. measure: direct (bought per EUR of output) or chain (GVA along the whole supply chain
  // per EUR of final demand for its products). level: country_sector, country, sector. home = in the buyer's country.
  profileSuppliers(countries, sectors = ['All sectors'], measure = 'direct', level = 'country_sector') {
    const { n, nc, ns } = this, P = this.profileRows(countries, sectors);
    const X = sum(P.map(i => this.x[i]));
    const nsrc = level === 'country' ? nc : level === 'sector' ? ns : n;
    const srcOf = i => level === 'country' ? this.cOf(i) : level === 'sector' ? this.sOf(i) : i;
    const home = new Float64Array(nsrc), abroad = new Float64Array(nsrc);
    if (measure === 'direct') {
      this.requireA();
      const { ptr, idx, val } = this.A;
      for (const j of P) {
        const w = this.x[j] / X, c = this.cOf(j);
        for (let p = ptr[j]; p < ptr[j + 1]; p++) { const i = idx[p]; (this.cOf(i) === c ? home : abroad)[srcOf(i)] += val[p] * w; }
      }
    } else {
      this.requireL();
      for (const j of P) {
        const w = this.x[j] / X, c = this.cOf(j), off = j * n;
        for (let i = 0; i < n; i++) (this.cOf(i) === c ? home : abroad)[srcOf(i)] += this.v[i] * this.L[off + i] * w;
      }
    }
    const out = [];
    for (const [isHome, a] of [[true, home], [false, abroad]]) {
      for (let s = 0; s < nsrc; s++) {
        if (!a[s]) continue;
        const source = level === 'country' ? this.countryCodes[s] : level === 'sector' ? this.sectorCodes[s] : `${this.countryOf(s)}_${this.sectorOf(s)}`;
        const row = { source, home: isHome, share: a[s],
                      source_name: level === 'country' ? this.countryNames[source] : level === 'sector' ? this.sectorNames[source] : this.label(s) };
        if (level !== 'sector') row.source_country = level === 'country' ? source : this.countryOf(s);
        out.push(row);
      }
    }
    return out.sort((a, b) => b.share - a.share);
  }
}

// The figures of the industry profile, in this order: name, label, format (as profile_labels in R/engine.R)
export const PROFILE_LABELS = [
  ['output', 'Output, EUR m', 'eur'],
  ['va', 'Gross value added (GVA), EUR m', 'eur'],
  ['va_share_country', 'Share of the country’s GVA', 'pct'],
  ['va_per_output', 'GVA per EUR of output', 'pct'],
  ['wage_share', 'Compensation of employees, % of GVA', 'pct'],
  ['gos_output', 'Gross operating surplus and mixed income, % of output', 'pct'],
  ['exports', 'Exports, % of output', 'pct'],
  ['exports_final', 'Of which final products, % of output', 'pct'],
  ['imported_inputs', 'Imported inputs, % of output (bought directly)', 'pct'],
  ['home_va_chain', 'Home GVA per EUR of final demand for its products (whole supply chain)', 'pct'],
  ['foreign_va_chain', 'Foreign GVA per EUR of final demand for its products (whole supply chain)', 'pct'],
  ['output_mult', 'Home output per EUR of final demand for its products', 'dec'],
  ['va_abroad', 'Its GVA that ends up in final demand abroad, % of GVA', 'pct'],
  ['va_gov', 'Its GVA that depends on government consumption (home and abroad), % of GVA', 'pct'],
  ['energy_direct', 'Energy bought directly, % of output', 'pct'],
  ['energy_total', 'Energy cost, direct and through suppliers, % of output', 'pct'],
  ['energy10_va', 'Energy prices +10% worldwide: extra cost, % of GVA', 'pct'],
  ['energy10_gos', 'Energy prices +10% worldwide: extra cost, % of gross operating surplus', 'pct'],
  ['upstream', 'Production steps to final users (upstreamness; 1 = sells only to final users)', 'dec']
];

// ---- Helpers -------------------------------------------------------------------------------------------
export function sum(a) { let s = 0; for (const x of a) s += x; return s; }
export function range(a, b) { return Array.from({ length: b - a }, (_, k) => a + k); }

// Solve M z = b (M: m x m, row by row) by Gaussian elimination with partial pivoting
export function solveDense(M, m, b) {
  const A = Float64Array.from(M), z = Float64Array.from(b);
  for (let col = 0; col < m; col++) {
    let piv = col, best = Math.abs(A[col * m + col]);
    for (let r = col + 1; r < m; r++) { const v = Math.abs(A[r * m + col]); if (v > best) { best = v; piv = r; } }
    if (best === 0) throw new Error('Singular system');
    if (piv !== col) {
      for (let c = 0; c < m; c++) { const t = A[col * m + c]; A[col * m + c] = A[piv * m + c]; A[piv * m + c] = t; }
      const t = z[col]; z[col] = z[piv]; z[piv] = t;
    }
    const d = A[col * m + col];
    for (let r = col + 1; r < m; r++) {
      const f = A[r * m + col] / d;
      if (!f) continue;
      for (let c = col; c < m; c++) A[r * m + c] -= f * A[col * m + c];
      z[r] -= f * z[col];
    }
  }
  for (let r = m - 1; r >= 0; r--) {
    let s = z[r];
    for (let c = r + 1; c < m; c++) s -= A[r * m + c] * z[c];
    z[r] = s / A[r * m + r];
  }
  return z;
}

// ---- Reading the binary files (shared by the browser and the Node tests) ---------------------------------
export function parseCore(meta, buffer) {
  const all = new Float32Array(buffer), core = {};
  for (const [name, p] of Object.entries(meta.core)) core[name] = all.subarray(p.offset, p.offset + p.length);
  return core;
}
export function parseL(buffer) { return new Float32Array(buffer); }
export function parseA(meta, buffer) {
  const n = meta.n_countries * meta.n_sectors, nnz = meta.A.nnz;
  const ptr = new Int32Array(buffer, 0, n + 1);
  const idxBytes = 4 * (n + 1);
  const idx = new Uint16Array(buffer, idxBytes, nnz);
  const valBytes = idxBytes + 2 * (nnz + (nnz % 2));
  const val = new Float32Array(buffer, valBytes, nnz);
  return { ptr, idx, val };
}
