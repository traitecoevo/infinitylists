// Small dependency-free data table: search, column sort, pagination.

const esc = (s) => String(s ?? '').replace(/[&<>"']/g, (c) => (
  { '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' }[c]))

export { esc }

/**
 * @typedef {Object} Column
 * @property {string} key       record field used for sorting and searching
 * @property {string} label
 * @property {'text'|'num'|'date'} [type]
 * @property {(row: any) => string} [html]  cell HTML (must escape its own content)
 * @property {(row: any) => string} [text]  plain cell text (escaped automatically)
 */

export class DataTable {
  /**
   * @param {HTMLElement} root
   * @param {{pageSize?: number}} [opts]
   */
  constructor(root, opts = {}) {
    this.root = root
    this.pageSize = opts.pageSize || 25
    this.columns = []
    this.rows = []
    this.view = []
    this.page = 0
    this.query = ''
    this.sort = null

    root.innerHTML = `
      <div class="dt-top">
        <label>Search: <input type="search" class="dt-search" aria-label="Search table"></label>
      </div>
      <div class="dt-scroll"><table class="dt"><thead></thead><tbody></tbody></table></div>
      <div class="dt-bottom">
        <span class="dt-info" aria-live="polite"></span>
        <span class="dt-pager">
          <button type="button" class="dt-prev">Previous</button>
          <span class="dt-page"></span>
          <button type="button" class="dt-next">Next</button>
        </span>
      </div>`
    this.thead = root.querySelector('thead')
    this.tbody = root.querySelector('tbody')
    this.info = root.querySelector('.dt-info')
    this.pageLabel = root.querySelector('.dt-page')
    this.prev = root.querySelector('.dt-prev')
    this.next = root.querySelector('.dt-next')

    let timer
    root.querySelector('.dt-search').addEventListener('input', (e) => {
      clearTimeout(timer)
      timer = setTimeout(() => { this.query = e.target.value.trim().toLowerCase(); this.page = 0; this.refresh() }, 150)
    })
    this.prev.addEventListener('click', () => { this.page--; this.renderBody() })
    this.next.addEventListener('click', () => { this.page++; this.renderBody() })
    this.thead.addEventListener('click', (e) => {
      const th = e.target.closest('th')
      if (!th) return
      const key = th.dataset.key
      const col = this.columns.find((c) => c.key === key)
      const defaultDir = col.type === 'text' || !col.type ? 'asc' : 'desc'
      this.sort = this.sort && this.sort.key === key
        ? { key, dir: this.sort.dir === 'asc' ? 'desc' : 'asc' }
        : { key, dir: defaultDir }
      this.page = 0
      this.refresh()
    })
  }

  /**
   * @param {Column[]} columns
   * @param {any[]} rows
   * @param {{key: string, dir: 'asc'|'desc'}} sort
   */
  setData(columns, rows, sort) {
    const sameColumns = this.columns.length === columns.length &&
      this.columns.every((c, i) => c.key === columns[i].key)
    this.columns = columns
    this.rows = rows
    if (!sameColumns || !this.sort) this.sort = sort
    this.page = 0
    this.refresh()
  }

  refresh() {
    const { columns, query } = this
    let view = this.rows
    if (query) {
      const keys = columns.map((c) => c.key)
      view = view.filter((r) => keys.some((k) => r[k] != null && String(r[k]).toLowerCase().includes(query)))
    }
    if (this.sort) {
      const { key, dir } = this.sort
      const col = columns.find((c) => c.key === key)
      const sign = dir === 'asc' ? 1 : -1
      const numeric = col && (col.type === 'num' || col.type === 'date')
      view = [...view].sort((a, b) => {
        const x = a[key], y = b[key]
        if (x == null && y == null) return 0
        if (x == null) return 1 // missing values always last
        if (y == null) return -1
        return sign * (numeric ? x - y : String(x).localeCompare(String(y)))
      })
    }
    this.view = view
    this.renderHead()
    this.renderBody()
  }

  renderHead() {
    this.thead.innerHTML = '<tr>' + this.columns.map((c) => {
      const active = this.sort && this.sort.key === c.key
      const aria = active ? (this.sort.dir === 'asc' ? 'ascending' : 'descending') : 'none'
      const arrow = active ? (this.sort.dir === 'asc' ? ' ▲' : ' ▼') : ''
      return `<th data-key="${esc(c.key)}" aria-sort="${aria}" tabindex="0">${esc(c.label)}${arrow}</th>`
    }).join('') + '</tr>'
  }

  renderBody() {
    const total = this.view.length
    const pages = Math.max(1, Math.ceil(total / this.pageSize))
    this.page = Math.min(Math.max(this.page, 0), pages - 1)
    const start = this.page * this.pageSize
    const slice = this.view.slice(start, start + this.pageSize)
    this.tbody.innerHTML = slice.length
      ? slice.map((r) => '<tr>' + this.columns.map((c) =>
          `<td>${c.html ? c.html(r) : esc(c.text ? c.text(r) : r[c.key])}</td>`).join('') + '</tr>').join('')
      : `<tr><td class="dt-empty" colspan="${this.columns.length}">No records</td></tr>`
    const filtered = this.query ? ` (filtered from ${this.rows.length.toLocaleString()} total entries)` : ''
    this.info.textContent = total
      ? `Showing ${(start + 1).toLocaleString()} to ${(start + slice.length).toLocaleString()} of ${total.toLocaleString()} entries${filtered}`
      : `Showing 0 entries${filtered}`
    this.pageLabel.textContent = `Page ${this.page + 1} of ${pages}`
    this.prev.disabled = this.page === 0
    this.next.disabled = this.page >= pages - 1
  }
}
