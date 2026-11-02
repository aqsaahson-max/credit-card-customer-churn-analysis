/* ═══════════════════════════════════════════════════════════════════════
   Customer Retention Decision-Support Dashboard — Application Logic
   ═══════════════════════════════════════════════════════════════════════ */

// ── Global State ──────────────────────────────────────────────────────
const APP = {
  records: [],          // All 2,026 test records
  summary: {},          // Dataset summary statistics
  reviews: {},          // Demo ID → { status, decision, note }
  currentPage: 1,
  pageSize: 50,
  sortCol: 'Attrition_Score',
  sortDir: 'desc',
  filterFlag: 'all',
  filterStatus: 'all',
  searchQuery: '',
  charts: {},           // Chart.js instances
};

// ── Constants ─────────────────────────────────────────────────────────
const THRESHOLD = 0.339210811098011;
const REVIEW_DECISIONS = [
  { key: 'info-needed',    label: 'Further information needed',    icon: '🔍' },
  { key: 'service-review', label: 'Suitable for a service review', icon: '📞' },
  { key: 'no-action',      label: 'No action proposed',           icon: '➖' },
  { key: 'escalated',      label: 'Escalate for specialist review',icon: '⬆️' },
];

// ── Helpers ───────────────────────────────────────────────────────────
const $ = (sel) => document.querySelector(sel);
const $$ = (sel) => document.querySelectorAll(sel);
const numeric = n => n !== null && n !== undefined && String(n).trim() !== '' && Number.isFinite(Number(n)) ? Number(n) : null;
const fmt = (n, d = 0) => numeric(n) === null ? 'Unavailable' : Number(n).toLocaleString('en-US', { minimumFractionDigits: d, maximumFractionDigits: d });
const escapeHTML = value => String(value).replace(/[&<>"']/g, c => ({'&':'&amp;', '<':'&lt;', '>':'&gt;', '"':'&quot;', "'":'&#39;'}[c]));
function finiteMean(records, field) {
  const values = records.map(r => numeric(r[field])).filter(v => v !== null);
  return values.length ? values.reduce((sum, v) => sum + v / values.length, 0) : null;
}
const fmtPct = (n) => (n * 100).toFixed(1) + '%';
const fmtScore = (n) => Number(n).toFixed(4);

function parseCSV(text) {
  const lines = text.trim().split('\n');
  const headers = lines[0].replace(/\r/g, '').split(',').map(h => h.replace(/^"|"$/g, ''));
  return lines.slice(1).filter(l => l.trim()).map(line => {
    const vals = [];
    let current = '';
    let inQuote = false;
    for (const ch of line.replace(/\r/g, '')) {
      if (ch === '"') { inQuote = !inQuote; continue; }
      if (ch === ',' && !inQuote) { vals.push(current); current = ''; continue; }
      current += ch;
    }
    vals.push(current);
    const obj = {};
    headers.forEach((h, i) => {
      const v = vals[i] || '';
      const n = Number(v);
      obj[h] = v !== '' && !isNaN(n) ? n : v;
    });
    return obj;
  });
}

async function loadJSON(path) {
  const resp = await fetch(path);
  return resp.json();
}

async function loadCSV(path) {
  const resp = await fetch(path);
  const text = await resp.text();
  return parseCSV(text);
}

function getScoreColor(score) {
  if (score >= 0.7) return 'var(--accent-rose)';
  if (score >= THRESHOLD) return 'var(--accent-amber)';
  if (score >= 0.1) return 'var(--accent-blue)';
  return 'var(--accent-emerald)';
}

function getScoreBadge(score) {
  if (score >= 0.7) return '<span class="badge badge-high">High</span>';
  if (score >= THRESHOLD) return '<span class="badge badge-medium">Flagged</span>';
  return '<span class="badge badge-low">Below</span>';
}

function getStatusHTML(demoId) {
  const r = APP.reviews[demoId];
  if (!r) return '<span class="status-dot pending"></span>Pending';
  const cls = r.decision === 'service-review' ? 'reviewed'
    : r.decision === 'info-needed' ? 'info-needed'
    : r.decision === 'escalated' ? 'escalated'
    : 'no-action';
  const labels = { 'info-needed': 'Info Needed', 'service-review': 'Reviewed', 'no-action': 'No Action', 'escalated': 'Escalated' };
  return `<span class="status-dot ${cls}"></span>${labels[r.decision] || r.decision}`;
}

// ── Data Loading ──────────────────────────────────────────────────────
async function initApp() {
  try {
    [APP.records, APP.summary] = await Promise.all([
      loadJSON('data/test_records.json'),
      loadJSON('data/summary.json'),
    ]);

    // Parse numeric fields and add reviewStatus field
    APP.records.forEach(r => {
      r.reviewStatus = 'pending';
      r.Attrition_Score = numeric(r.Attrition_Score);
      r.Predicted_Churn = numeric(r.Predicted_Churn);
      r.Actual_Churn = numeric(r.Actual_Churn);
      r.Customer_Age = numeric(r.Customer_Age);
      r.Dependent_count = numeric(r.Dependent_count);
      r.Months_on_book = numeric(r.Months_on_book);
      r.Total_Relationship_Count = numeric(r.Total_Relationship_Count);
      r.Months_Inactive_12_mon = numeric(r.Months_Inactive_12_mon);
      r.Contacts_Count_12_mon = numeric(r.Contacts_Count_12_mon);
      r.Credit_Limit = numeric(r.Credit_Limit);
      r.Total_Revolving_Bal = numeric(r.Total_Revolving_Bal);
      r.Avg_Open_To_Buy = numeric(r.Avg_Open_To_Buy);
      r.Total_Amt_Chng_Q4_Q1 = numeric(r.Total_Amt_Chng_Q4_Q1);
      r.Total_Trans_Amt = numeric(r.Total_Trans_Amt);
      r.Total_Trans_Ct = numeric(r.Total_Trans_Ct);
      r.Total_Ct_Chng_Q4_Q1 = numeric(r.Total_Ct_Chng_Q4_Q1);
      r.Avg_Utilization_Ratio = numeric(r.Avg_Utilization_Ratio);
    });

    renderOverview();
    renderDescriptive();
    renderModelEvidence();
    renderQueue();
    renderCapacity();
    renderTracking();
    setupNavigation();
    setupDetailPanel();

  } catch (err) {
    console.error('Failed to load dashboard data:', err);
    $('.page-body').innerHTML = `
      <div class="callout callout-warning">
        <strong>Data loading failed.</strong> Ensure the dashboard is served via a local server (not opened as a file).
        <br><br>Run: <code>python -m http.server 8000</code> in the dashboard folder, then open <code>http://localhost:8000</code>
        <br><br>Error: ${err.message}
      </div>`;
  }
}

// ── Navigation ────────────────────────────────────────────────────────
function setupNavigation() {
  const titles = {
    overview:    ['Project Overview', 'Customer Retention Decision-Support Dashboard'],
    descriptive: ['Customer Activity & Descriptive Findings', 'Observed patterns from the source dataset (Task 3)'],
    model:       ['Model Evidence', 'Statistical modelling results and evaluation (Task 5)'],
    queue:       ['Review Queue', 'Demonstration records from the 2,026-record test subset'],
    capacity:    ['Workload & Capacity', 'Explore review capacity and threshold trade-offs'],
    tracking:    ['Simulated Review Tracking', 'Demonstration activity from this session'],
    pilot:       ['Pilot Proposal & Expert Feedback', 'Proposed evaluation framework (Tasks 6, 10, 11)'],
    methodology: ['Methodology & Limitations', 'Analytical approach and critical evaluations (Tasks 7–9)'],
    recommendations: ['Final Recommendations', 'Strategic, operational, and pilot decision criteria (Task 12)'],
  };

  $$('.nav-item').forEach(item => {
    item.addEventListener('click', () => {
      const screen = item.dataset.screen;
      $$('.nav-item').forEach(n => n.classList.remove('active'));
      item.classList.add('active');
      $$('.screen').forEach(s => s.classList.remove('active'));
      $(`#screen-${screen}`).classList.add('active');
      const [title, sub] = titles[screen] || ['', ''];
      $('#pageTitle').textContent = title;
      $('#pageSubtitle').textContent = sub;

      // Close mobile menu
      $('#sidebar').classList.remove('mobile-open');
    });
  });

  // Mobile menu
  $('#mobileMenuBtn').addEventListener('click', () => {
    $('#sidebar').classList.toggle('mobile-open');
  });

  // Tabs
  document.addEventListener('click', (e) => {
    if (!e.target.classList.contains('tab-btn')) return;
    const tabGroup = e.target.closest('.tabs');
    const tabId = e.target.dataset.tab;
    tabGroup.querySelectorAll('.tab-btn').forEach(b => b.classList.remove('active'));
    e.target.classList.add('active');
    // Find sibling tab panels
    let sibling = tabGroup.nextElementSibling;
    while (sibling && sibling.classList.contains('tab-panel')) {
      sibling.classList.remove('active');
      if (sibling.id === `tab-${tabId}`) sibling.classList.add('active');
      sibling = sibling.nextElementSibling;
    }
  });
}

// ── Screen 1: Overview ────────────────────────────────────────────────
function renderOverview() {
  const s = APP.summary;
  $('#totalRecords').textContent = fmt(s.total_records);
  $('#trainCount').textContent = fmt(s.training_count);
  $('#testCount').textContent = fmt(s.test_count);

  $('#overviewMetrics').innerHTML = `
    <div class="metric-card" data-accent="blue">
      <div class="metric-label">Source Dataset</div>
      <div class="metric-value">${fmt(s.total_records)}</div>
      <div class="metric-sub">Total customer records</div>
    </div>
    <div class="metric-card" data-accent="rose">
      <div class="metric-label">Recorded Attrition</div>
      <div class="metric-value">${s.attrition_rate}%</div>
      <div class="metric-sub">${fmt(s.total_attrited)} of ${fmt(s.total_records)} records</div>
    </div>
    <div class="metric-card" data-accent="amber">
      <div class="metric-label">Test Records Flagged</div>
      <div class="metric-value">${fmt(s.test_flagged)}</div>
      <div class="metric-sub">At threshold ≥ ${THRESHOLD.toFixed(4)}</div>
    </div>
    <div class="metric-card" data-accent="cyan">
      <div class="metric-label">Selected Model</div>
      <div class="metric-value" style="font-size:16px">Spline Logistic</div>
      <div class="metric-sub">ROC-AUC 0.960 on test set</div>
    </div>
  `;
}

// ── Screen 2: Descriptive Findings ────────────────────────────────────
async function renderDescriptive() {
  // Load comparison data
  const comparison = await loadCSV('data/Numeric_Comparison_By_Attrition.csv');
  const categories = await loadCSV('data/Attrition_By_Category.csv');
  const inference  = await loadCSV('data/Task4_Primary_Tests_Holm_Adjusted.csv');

  // Attrition distribution chart
  const s = APP.summary;
  renderChart('chartAttrition', 'doughnut', {
    labels: ['Existing Customer', 'Attrited Customer'],
    datasets: [{
      data: [s.total_existing, s.total_attrited],
      backgroundColor: ['rgba(203, 213, 225, 0.8)', 'rgba(244, 126, 126, 0.8)'],
      borderColor: ['rgba(148, 163, 184, 1)', 'rgba(244, 126, 126, 1)'],
      borderWidth: 2,
    }]
  }, {
    cutout: '60%',
    plugins: {
      legend: { position: 'bottom', labels: { color: '#94a3b8', padding: 16 } },
    }
  });

  // Transaction count by status
  const transData = comparison.find(r => r.Variable === 'Total_Trans_Ct');
  if (transData) {
    renderChart('chartTransCount', 'bar', {
      labels: ['Existing', 'Attrited'],
      datasets: [{
        label: 'Mean Transaction Count',
        data: [transData.Existing_Mean, transData.Attrited_Mean],
        backgroundColor: ['rgba(203, 213, 225, 0.8)', 'rgba(244, 126, 126, 0.8)'],
        borderColor: ['rgba(148, 163, 184, 1)', 'rgba(244, 126, 126, 1)'],
        borderWidth: 2,
        borderRadius: 6,
      }]
    }, {
      plugins: { legend: { display: false } },
      scales: {
        y: { beginAtZero: true, grid: { color: 'rgba(148,163,184,0.06)' }, ticks: { color: '#64748b' } },
        x: { grid: { display: false }, ticks: { color: '#94a3b8' } },
      }
    });
  }

  // Contacts chart
  const contactData = categories.filter(r => r.Variable === 'Contacts_Count_12_mon');
  if (contactData.length) {
    renderChart('chartContacts', 'bar', {
      labels: contactData.map(r => r.Category + ' contacts'),
      datasets: [{
        label: 'Attrition Rate (%)',
        data: contactData.map(r => r.Attrition_Percent),
        backgroundColor: contactData.map(r => r.Attrition_Percent > 20 ? 'rgba(244,126,126,0.8)' : 'rgba(203,213,225,0.8)'),
        borderColor: contactData.map(r => r.Attrition_Percent > 20 ? 'rgba(244,126,126,1)' : 'rgba(148,163,184,1)'),
        borderWidth: 2,
        borderRadius: 6,
      }]
    }, {
      plugins: { legend: { display: false } },
      scales: {
        y: { beginAtZero: true, max: 105, grid: { color: 'rgba(148,163,184,0.06)' }, ticks: { color: '#64748b', callback: v => v + '%' } },
        x: { grid: { display: false }, ticks: { color: '#94a3b8' } },
      }
    });
  }

  // Relationships chart
  const relData = categories.filter(r => r.Variable === 'Total_Relationship_Count');
  if (relData.length) {
    renderChart('chartRelationships', 'bar', {
      labels: relData.map(r => r.Category + ' products'),
      datasets: [{
        label: 'Attrition Rate (%)',
        data: relData.map(r => r.Attrition_Percent),
        backgroundColor: relData.map(r => r.Attrition_Percent > 20 ? 'rgba(244,126,126,0.8)' : 'rgba(203,213,225,0.8)'),
        borderColor: relData.map(r => r.Attrition_Percent > 20 ? 'rgba(244,126,126,1)' : 'rgba(148,163,184,1)'),
        borderWidth: 2,
        borderRadius: 6,
      }]
    }, {
      plugins: { legend: { display: false } },
      scales: {
        y: { beginAtZero: true, grid: { color: 'rgba(148,163,184,0.06)' }, ticks: { color: '#64748b', callback: v => v + '%' } },
        x: { grid: { display: false }, ticks: { color: '#94a3b8' } },
      }
    });
  }

  // Numeric comparison table
  const compCols = ['Variable', 'Existing_Mean', 'Attrited_Mean', 'Existing_SD', 'Attrited_SD'];
  const compLabels = { Variable: 'Variable', Existing_Mean: 'Existing Mean', Attrited_Mean: 'Attrited Mean', Existing_SD: 'Existing SD', Attrited_SD: 'Attrited SD' };
  $('#comparisonTableContainer').innerHTML = buildTable(comparison, compCols, compLabels, r => ({
    Variable: r.Variable.replace(/_/g, ' '),
    Existing_Mean: typeof r.Existing_Mean === 'number' ? r.Existing_Mean.toFixed(2) : r.Existing_Mean,
    Attrited_Mean: typeof r.Attrited_Mean === 'number' ? r.Attrited_Mean.toFixed(2) : r.Attrited_Mean,
    Existing_SD: typeof r.Existing_SD === 'number' ? r.Existing_SD.toFixed(2) : r.Existing_SD,
    Attrited_SD: typeof r.Attrited_SD === 'number' ? r.Attrited_SD.toFixed(2) : r.Attrited_SD,
  }));

  // Inference table
  const infCols = ['Analysis', 'Method', 'Holm_p_display', 'Decision_at_5_percent'];
  const infLabels = { Analysis: 'Analysis', Method: 'Test Method', Holm_p_display: 'Holm-Adjusted p', Decision_at_5_percent: 'Decision (α=0.05)' };
  $('#inferenceTableContainer').innerHTML = buildTable(inference, infCols, infLabels);
}

// ── Screen 3: Model Evidence ──────────────────────────────────────────
async function renderModelEvidence() {
  const models = await loadCSV('data/OOF_Model_Comparison.csv');
  const testPerf = await loadCSV('data/Final_Test_Probability_Performance.csv');
  const metrics = await loadCSV('data/Final_Test_Metric_Intervals.csv');
  const calibration = await loadCSV('data/Final_Test_Calibration_Table.csv');
  const thresholds = await loadCSV('data/Final_Test_Threshold_Comparison.csv');

  // Test metrics cards
  if (testPerf[0]) {
    const t = testPerf[0];
    $('#testAUC').textContent = Number(t.ROC_AUC).toFixed(4);
    $('#testLogLoss').textContent = Number(t.Log_Loss).toFixed(4);
    $('#testAP').textContent = Number(t.Average_Precision).toFixed(4);
    $('#testBrier').textContent = Number(t.Brier_Score).toFixed(4);
  }

  // Model comparison chart
  const sortedModels = [...models].sort((a, b) => a.Log_Loss - b.Log_Loss);
  renderChart('chartModelComparison', 'bar', {
    labels: sortedModels.map(m => m.Model),
    datasets: [
      {
        label: 'Log Loss (lower is better)',
        data: sortedModels.map(m => m.Log_Loss),
        backgroundColor: sortedModels.map(m => m.Model === 'Spline' ? 'rgba(98,196,153,0.8)' : 'rgba(203,213,225,0.8)'),
        borderColor: sortedModels.map(m => m.Model === 'Spline' ? 'rgba(98,196,153,1)' : 'rgba(148,163,184,1)'),
        borderWidth: 2,
        borderRadius: 6,
      }
    ]
  }, {
    indexAxis: 'y',
    plugins: { legend: { display: false } },
    scales: {
      x: { grid: { color: 'rgba(148,163,184,0.06)' }, ticks: { color: '#64748b' } },
      y: { grid: { display: false }, ticks: { color: '#94a3b8', font: { size: 11 } } },
    }
  });

  // Model comparison table
  const modCols = ['Model', 'Log_Loss', 'Brier_Score', 'ROC_AUC', 'Average_Precision'];
  const modLabels = { Model: 'Model', Log_Loss: 'Log Loss', Brier_Score: 'Brier Score', ROC_AUC: 'ROC-AUC', Average_Precision: 'Avg Precision' };
  $('#modelComparisonTableContainer').innerHTML = buildTable(sortedModels, modCols, modLabels, r => ({
    Model: r.Model,
    Log_Loss: Number(r.Log_Loss).toFixed(4),
    Brier_Score: Number(r.Brier_Score).toFixed(4),
    ROC_AUC: Number(r.ROC_AUC).toFixed(4),
    Average_Precision: Number(r.Average_Precision).toFixed(4),
  }));

  // Metrics table with CI
  const metCols = ['Metric', 'Estimate', 'Lower_95', 'Upper_95'];
  const metLabels = { Metric: 'Metric', Estimate: 'Estimate', Lower_95: 'Lower 95% CI', Upper_95: 'Upper 95% CI' };
  $('#metricsTableContainer').innerHTML = buildTable(metrics, metCols, metLabels, r => ({
    Metric: r.Metric,
    Estimate: fmtPct(r.Estimate),
    Lower_95: fmtPct(r.Lower_95),
    Upper_95: fmtPct(r.Upper_95),
  }));

  // Calibration chart
  renderChart('chartCalibration', 'bar', {
    labels: calibration.map((_, i) => `Decile ${i + 1}`),
    datasets: [
      {
        label: 'Predicted Probability',
        data: calibration.map(r => r.Mean_Predicted),
        backgroundColor: 'rgba(203, 213, 225, 0.8)',
        borderColor: 'rgba(148, 163, 184, 1)',
        borderWidth: 2,
        borderRadius: 4,
      },
      {
        label: 'Observed Proportion',
        data: calibration.map(r => r.Observed_Proportion),
        backgroundColor: 'rgba(98, 196, 153, 0.8)',
        borderColor: 'rgba(98, 196, 153, 1)',
        borderWidth: 2,
        borderRadius: 4,
      }
    ]
  }, {
    plugins: { legend: { position: 'bottom', labels: { color: '#94a3b8', padding: 16 } } },
    scales: {
      y: { beginAtZero: true, max: 1, grid: { color: 'rgba(148,163,184,0.06)' }, ticks: { color: '#64748b', callback: v => (v * 100) + '%' } },
      x: { grid: { display: false }, ticks: { color: '#94a3b8', font: { size: 10 } } },
    }
  });

  // Calibration table
  const calCols = ['Group', 'N', 'Mean_Predicted', 'Observed_Proportion', 'Lower_95', 'Upper_95'];
  const calLabels = { Group: 'Decile', N: 'N', Mean_Predicted: 'Predicted', Observed_Proportion: 'Observed', Lower_95: 'Lower 95%', Upper_95: 'Upper 95%' };
  $('#calibrationTableContainer').innerHTML = buildTable(calibration, calCols, calLabels, r => ({
    Group: r.Group,
    N: r.N,
    Mean_Predicted: Number(r.Mean_Predicted).toFixed(4),
    Observed_Proportion: Number(r.Observed_Proportion).toFixed(4),
    Lower_95: Number(r.Lower_95).toFixed(4),
    Upper_95: Number(r.Upper_95).toFixed(4),
  }));

  // Threshold table
  const thCols = ['Rule', 'Threshold', 'TP', 'FP', 'TN', 'FN', 'Precision', 'Recall', 'F1', 'Flagged_Percent'];
  const thLabels = { Rule: 'Rule', Threshold: 'Threshold', TP: 'TP', FP: 'FP', TN: 'TN', FN: 'FN', Precision: 'Precision', Recall: 'Recall', F1: 'F1', Flagged_Percent: 'Flagged %' };
  $('#thresholdTableContainer').innerHTML = buildTable(thresholds, thCols, thLabels, r => ({
    Rule: r.Rule,
    Threshold: Number(r.Threshold).toFixed(4),
    TP: r.TP, FP: r.FP, TN: r.TN, FN: r.FN,
    Precision: fmtPct(r.Precision),
    Recall: fmtPct(r.Recall),
    F1: Number(r.F1).toFixed(4),
    Flagged_Percent: Number(r.Flagged_Percent).toFixed(1) + '%',
  }));
}

// ── Screen 4: Review Queue ────────────────────────────────────────────
function renderQueue() {
  const search = $('#queueSearch');
  const filterFlag = $('#queueFilter');
  const filterStatus = $('#queueStatusFilter');
  const resetBtn = $('#resetReviews');

  search.addEventListener('input', () => { APP.searchQuery = search.value.trim().toUpperCase(); APP.currentPage = 1; updateQueueTable(); });
  filterFlag.addEventListener('change', () => { APP.filterFlag = filterFlag.value; APP.currentPage = 1; updateQueueTable(); });
  filterStatus.addEventListener('change', () => { APP.filterStatus = filterStatus.value; APP.currentPage = 1; updateQueueTable(); });
  resetBtn.addEventListener('click', () => {
    if (confirm('Reset all simulated review decisions?')) {
      APP.reviews = {};
      closeDetail();
      $('#detailBody').innerHTML = '';
      APP.sortCol = 'Attrition_Score'; APP.sortDir = 'desc'; APP.currentPage = 1;
      APP.filterFlag = 'all'; APP.filterStatus = 'all'; APP.searchQuery = '';
      search.value = ''; filterFlag.value = 'all'; filterStatus.value = 'all';
      APP.records.forEach(r => { r.reviewStatus = 'pending'; });
      updateQueueTable();
      renderTracking();
    }
  });

  // Sort handlers
  $$('#queueTable th').forEach(th => {
    th.addEventListener('click', () => {
      const col = th.dataset.sort;
      if (!col) return;
      if (APP.sortCol === col) APP.sortDir = APP.sortDir === 'asc' ? 'desc' : 'asc';
      else { APP.sortCol = col; APP.sortDir = col === 'Attrition_Score' ? 'desc' : 'asc'; }
      updateQueueTable();
    });
  });

  // Pagination
  $('#prevPage').addEventListener('click', () => { if (APP.currentPage > 1) { APP.currentPage--; updateQueueTable(); } });
  $('#nextPage').addEventListener('click', () => {
    const filtered = getFilteredRecords();
    const maxPage = Math.ceil(filtered.length / APP.pageSize);
    if (APP.currentPage < maxPage) { APP.currentPage++; updateQueueTable(); }
  });

  updateQueueTable();
}

function getFilteredRecords() {
  return APP.records.filter(r => {
    if (APP.filterFlag === 'flagged' && r.Attrition_Score < THRESHOLD) return false;
    if (APP.filterFlag === 'unflagged' && r.Attrition_Score >= THRESHOLD) return false;
    if (APP.filterStatus !== 'all') {
      const review = APP.reviews[r.Demo_ID];
      const status = review ? review.decision : 'pending';
      // Map decisions to filter values
      const statusMap = { 'service-review': 'reviewed', 'info-needed': 'info-needed', 'no-action': 'no-action', 'escalated': 'escalated' };
      const mappedStatus = review ? (statusMap[review.decision] || 'pending') : 'pending';
      if (APP.filterStatus !== mappedStatus) return false;
    }
    if (APP.searchQuery && !r.Demo_ID.toUpperCase().includes(APP.searchQuery)) return false;
    return true;
  });
}

function updateQueueTable() {
  const filtered = getFilteredRecords();

  // Sort
  filtered.sort((a, b) => {
    let va = a[APP.sortCol], vb = b[APP.sortCol];
    if (APP.sortCol === 'reviewStatus') {
      va = APP.reviews[a.Demo_ID] ? APP.reviews[a.Demo_ID].decision : 'pending';
      vb = APP.reviews[b.Demo_ID] ? APP.reviews[b.Demo_ID].decision : 'pending';
    }
    if (typeof va === 'string') return APP.sortDir === 'asc' ? va.localeCompare(vb) : vb.localeCompare(va);
    return APP.sortDir === 'asc' ? va - vb : vb - va;
  });

  // Paginate
  const totalPages = Math.max(1, Math.ceil(filtered.length / APP.pageSize));
  APP.currentPage = Math.min(APP.currentPage, totalPages);
  const start = (APP.currentPage - 1) * APP.pageSize;
  const page = filtered.slice(start, start + APP.pageSize);

  // Update sort indicators
  $$('#queueTable th').forEach(th => {
    th.classList.remove('sorted-asc', 'sorted-desc');
    if (th.dataset.sort === APP.sortCol) th.classList.add(`sorted-${APP.sortDir}`);
  });

  const activeHeader = [...$$('#queueTable th')].find(th => th.dataset.sort === APP.sortCol);
  $('#queueSort').textContent = 'Active sort: ' + activeHeader.textContent + ' — ' + (APP.sortDir === 'desc' ? 'descending' : 'ascending');
  // Render rows
  const tbody = $('#queueTableBody');
  if (page.length === 0) {
    tbody.innerHTML = '<tr><td colspan="7" style="text-align:center; color:var(--text-muted); padding:32px">No matching records</td></tr>';
  } else {
    tbody.innerHTML = page.map(r => {
      const isFlagged = r.Attrition_Score >= THRESHOLD;
      const scoreColor = getScoreColor(r.Attrition_Score);
      return `<tr class="clickable-row ${isFlagged ? 'row-flagged' : ''}" data-id="${r.Demo_ID}">
        <td><span class="text-mono" style="font-size:12px">${r.Demo_ID}</span></td>
        <td>
          <div class="score-bar">
            <div class="score-bar-track"><div class="score-bar-fill" style="width:${r.Attrition_Score * 100}%; background:${scoreColor}"></div></div>
            <span class="score-bar-value" style="color:${scoreColor}">${fmtScore(r.Attrition_Score)}</span>
          </div>
        </td>
        <td>${r.Total_Trans_Ct}</td>
        <td>${fmt(r.Total_Trans_Amt)}</td>
        <td>${r.Months_Inactive_12_mon}</td>
        <td>${r.Total_Relationship_Count}</td>
        <td>${getStatusHTML(r.Demo_ID)}</td>
      </tr>`;
    }).join('');

    // Row click handlers
    tbody.querySelectorAll('.clickable-row').forEach(row => {
      row.addEventListener('click', () => openDetail(row.dataset.id));
    });
  }

  // Pagination info
  $('#paginationInfo').textContent = `${filtered.length ? start + 1 : 0}–${Math.min(start + APP.pageSize, filtered.length)} of ${filtered.length} records`;

  // Update badge
  const flaggedCount = APP.records.filter(r => r.Attrition_Score >= THRESHOLD).length;
  $('#queueBadge').textContent = flaggedCount;
}

// ── Screen 5: Capacity ────────────────────────────────────────────────
function renderCapacity() {
  const slider = $('#capacitySlider');
  const valDisplay = $('#capacityValue');

  function updateCapacity() {
    const cap = Math.max(0, Number(slider.value) || 0);
    valDisplay.textContent = cap;

    const flagged = APP.records.filter(r => r.Attrition_Score >= THRESHOLD).length;
    const within = Math.min(cap, flagged);
    const excess = Math.max(0, flagged - cap);
    const util = cap > 0 ? (within / cap) * 100 : 0;

    $('#capFlagged').textContent = fmt(flagged);
    $('#capWithin').textContent = fmt(within);
    $('#capExcess').textContent = excess > 0 ? fmt(excess) : '0';
    $('#capUtil').textContent = util.toFixed(0) + '%';
  }

  slider.addEventListener('input', updateCapacity);
  updateCapacity();

  // Build threshold comparison table for capacity screen
  const thresholds = [0.2, 0.3, THRESHOLD, 0.4, 0.5, 0.6, 0.7, 0.8];
  const rows = thresholds.map(t => {
    const flagged = APP.records.filter(r => r.Attrition_Score >= t);
    const tp = flagged.filter(r => r.Actual_Churn === 1).length;
    const fp = flagged.filter(r => r.Actual_Churn === 0).length;
    const fn = APP.records.filter(r => r.Actual_Churn === 1 && r.Attrition_Score < t).length;
    const precision = flagged.length > 0 ? tp / flagged.length : 0;
    const totalAttrited = APP.records.filter(r => r.Actual_Churn === 1).length;
    const recall = totalAttrited > 0 ? tp / totalAttrited : 0;
    const isPrimary = Math.abs(t - THRESHOLD) < 0.001;
    return { threshold: t, flagged: flagged.length, tp, fp, fn, precision, recall, isPrimary };
  });

  let tableHTML = '<table class="data-table"><thead><tr><th>Threshold</th><th>Flagged</th><th>TP</th><th>FP</th><th>FN</th><th>Precision</th><th>Recall</th></tr></thead><tbody>';
  rows.forEach(r => {
    const style = r.isPrimary ? ' style="background:rgba(59,130,246,0.06)"' : '';
    const label = r.isPrimary ? ` <span class="badge badge-info">Selected</span>` : '';
    tableHTML += `<tr${style}>
      <td>${r.threshold.toFixed(4)}${label}</td>
      <td>${r.flagged}</td><td>${r.tp}</td><td>${r.fp}</td><td>${r.fn}</td>
      <td>${fmtPct(r.precision)}</td><td>${fmtPct(r.recall)}</td>
    </tr>`;
  });
  tableHTML += '</tbody></table>';
  $('#capacityThresholdTable').innerHTML = tableHTML;
}

// ── Screen 6: Review Tracking ─────────────────────────────────────────
function renderTracking() {
  const reviews = Object.entries(APP.reviews);
  const counts = { 'info-needed': 0, 'service-review': 0, 'no-action': 0, 'escalated': 0 };
  reviews.forEach(([_, r]) => { if (counts[r.decision] !== undefined) counts[r.decision]++; });
  const pending = APP.records.length - reviews.length;

  $('#reviewSummary').innerHTML = `
    <div class="review-summary-card">
      <div class="review-summary-count">${Math.max(0, pending)}</div>
      <div class="review-summary-label">Awaiting Review</div>
    </div>
    <div class="review-summary-card">
      <div class="review-summary-count" style="color:var(--accent-emerald)">${counts['service-review']}</div>
      <div class="review-summary-label">Service Review</div>
    </div>
    <div class="review-summary-card">
      <div class="review-summary-count" style="color:var(--accent-amber)">${counts['info-needed']}</div>
      <div class="review-summary-label">Info Needed</div>
    </div>
    <div class="review-summary-card">
      <div class="review-summary-count">${counts['no-action']}</div>
      <div class="review-summary-label">No Action</div>
    </div>
    <div class="review-summary-card">
      <div class="review-summary-count" style="color:var(--accent-violet)">${counts['escalated']}</div>
      <div class="review-summary-label">Escalated</div>
    </div>
  `;

  const tbody = $('#reviewedTableBody');
  if (reviews.length === 0) {
    tbody.innerHTML = '<tr><td colspan="5" style="text-align:center; color:var(--text-muted); padding:32px">No reviews recorded yet. Open a record from the Review Queue to begin.</td></tr>';
  } else {
    tbody.innerHTML = reviews.map(([id, r]) => {
      const record = APP.records.find(rec => rec.Demo_ID === id);
      const score = record ? fmtScore(record.Attrition_Score) : '—';
      const decisionLabel = REVIEW_DECISIONS.find(d => d.key === r.decision);
      return `<tr>
        <td><span class="text-mono" style="font-size:12px; cursor:pointer; color:var(--accent-blue)" onclick="openDetail('${id}')">${id}</span></td>
        <td>${score}</td>
        <td>${decisionLabel ? decisionLabel.icon + ' ' + decisionLabel.label : r.decision}</td>
        <td style="max-width:200px; overflow:hidden; text-overflow:ellipsis">${escapeHTML(r.note || '—')}</td>
        <td><button class="btn btn-ghost btn-sm" onclick="clearReview('${id}')">Clear</button></td>
      </tr>`;
    }).join('');
  }
}

window.clearReview = function(demoId) {
  delete APP.reviews[demoId];
  const record = APP.records.find(r => r.Demo_ID === demoId);
  if (record) record.reviewStatus = 'pending';
  renderTracking();
  updateQueueTable();
};

// ── Detail Panel ──────────────────────────────────────────────────────
function setupDetailPanel() {
  $('#detailClose').addEventListener('click', closeDetail);
  $('#detailOverlay').addEventListener('click', closeDetail);
  document.addEventListener('keydown', (e) => { if (e.key === 'Escape') closeDetail(); });
}

function closeDetail() {
  $('#detailPanel').classList.remove('open');
  $('#detailOverlay').classList.remove('open');
}

window.openDetail = function(demoId) {
  const record = APP.records.find(r => r.Demo_ID === demoId);
  if (!record) return;

  const review = APP.reviews[demoId] || null;
  const scoreColor = getScoreColor(record.Attrition_Score);
  const isFlagged = record.Attrition_Score >= THRESHOLD;

  // Compute population comparison stats safely
  const allRecords = APP.records;
  const transCtMean = finiteMean(allRecords, 'Total_Trans_Ct');
  const transAmtMean = finiteMean(allRecords, 'Total_Trans_Amt');
  const inactiveMean = finiteMean(allRecords, 'Months_Inactive_12_mon');
  const relMean = finiteMean(allRecords, 'Total_Relationship_Count');
  const transCtMax = allRecords.reduce((m, r) => Math.max(m, Number(r.Total_Trans_Ct) || 0), 0) || 1;
  const transAmtMax = allRecords.reduce((m, r) => Math.max(m, Number(r.Total_Trans_Amt) || 0), 0) || 1;

  $('#detailTitle').textContent = record.Demo_ID;

  let html = `
    <div class="detail-section">
      <div class="detail-section-title">Model Score</div>
      <div style="display:flex; align-items:center; gap:16px; margin-bottom:12px">
        <span style="font-size:32px; font-weight:800; color:${scoreColor}">${fmtScore(record.Attrition_Score)}</span>
        ${getScoreBadge(record.Attrition_Score)}
      </div>
      <div class="score-bar" style="max-width:300px">
        <div class="score-bar-track" style="height:10px; max-width:none">
          <div class="score-bar-fill" style="width:${record.Attrition_Score * 100}%; background:${scoreColor}"></div>
        </div>
      </div>
      <div class="text-sm text-muted mt-4">
        ${isFlagged ? 'This record is flagged at the training-selected threshold (≥ 0.3392).' : 'Below the training-selected threshold (< 0.3392).'}
        Model score for recorded attrition in this dataset. It has not been validated as a probability of future departure.
      </div>
    </div>

    <div class="detail-section">
      <div class="detail-section-title">Activity Measures</div>
      ${buildComparisonBar('Transaction Count', record.Total_Trans_Ct, transCtMean, transCtMax)}
      ${buildComparisonBar('Transaction Amount (dataset monetary units)', record.Total_Trans_Amt, transAmtMean, transAmtMax)}
      ${buildComparisonBar('Inactive Months', record.Months_Inactive_12_mon, inactiveMean, 6)}
      ${buildComparisonBar('Products Held', record.Total_Relationship_Count, relMean, 6)}
    </div>

    <div class="detail-section">
      <div class="detail-section-title">Customer Profile</div>
      <div class="detail-field"><span class="detail-field-label">Age</span><span class="detail-field-value">${record.Customer_Age}</span></div>
      <div class="detail-field"><span class="detail-field-label">Gender</span><span class="detail-field-value">${record.Gender}</span></div>
      <div class="detail-field"><span class="detail-field-label">Education</span><span class="detail-field-value">${record.Education_Level}</span></div>
      <div class="detail-field"><span class="detail-field-label">Marital Status</span><span class="detail-field-value">${record.Marital_Status}</span></div>
      <div class="detail-field"><span class="detail-field-label">Income Category</span><span class="detail-field-value">${escapeHTML(String(record.Income_Category).replaceAll("$", ""))} (dataset category; currency unverified)</span></div>
      <div class="detail-field"><span class="detail-field-label">Card Category</span><span class="detail-field-value">${record.Card_Category}</span></div>
      <div class="detail-field"><span class="detail-field-label">Months on Book</span><span class="detail-field-value">${record.Months_on_book}</span></div>
    </div>

    <div class="detail-section">
      <div class="detail-section-title">Financial Measures <span style="font-size:11px; font-weight:normal; color:var(--text-muted)">(dataset monetary units)</span></div>
      <div class="detail-field"><span class="detail-field-label">Credit Limit</span><span class="detail-field-value">${fmt(record.Credit_Limit)}</span></div>
      <div class="detail-field"><span class="detail-field-label">Revolving Balance</span><span class="detail-field-value">${fmt(record.Total_Revolving_Bal)}</span></div>
      <div class="detail-field"><span class="detail-field-label">Avg Open to Buy</span><span class="detail-field-value">${fmt(record.Avg_Open_To_Buy)}</span></div>
      <div class="detail-field"><span class="detail-field-label">Utilisation Ratio</span><span class="detail-field-value">${(record.Avg_Utilization_Ratio * 100).toFixed(1)}%</span></div>
      <div class="detail-field"><span class="detail-field-label">Amt Change Q4/Q1</span><span class="detail-field-value">${Number(record.Total_Amt_Chng_Q4_Q1).toFixed(3)}</span></div>
      <div class="detail-field"><span class="detail-field-label">Count Change Q4/Q1</span><span class="detail-field-value">${Number(record.Total_Ct_Chng_Q4_Q1).toFixed(3)}</span></div>
      <div class="detail-field"><span class="detail-field-label">Contacts (12 mo.)</span><span class="detail-field-value">${record.Contacts_Count_12_mon}</span></div>
      <div class="detail-field"><span class="detail-field-label">Dependents</span><span class="detail-field-value">${record.Dependent_count}</span></div>
    </div>

    <div class="callout callout-limitation" style="font-size:12px">
      <strong>Note:</strong> These activity measures describe what was recorded for this customer, not the reasons for the model's score. Showing a low transaction count does not prove it caused this individual's attrition risk.
    </div>

    <div class="detail-section">
      <div class="detail-section-title">Simulated Review Decision</div>
      <div class="review-actions" id="reviewActions">
        ${REVIEW_DECISIONS.map(d => `
          <div class="review-option ${review && review.decision === d.key ? 'selected' : ''}" data-decision="${d.key}">
            <div class="review-option-radio"></div>
            <span>${d.icon} ${d.label}</span>
          </div>
        `).join('')}
      </div>
      <textarea class="review-note" id="reviewNote" placeholder="Example: Additional customer context needed before considering contact.">${escapeHTML(review ? review.note || '' : '')}</textarea>
      <div style="display:flex; gap:8px; margin-top:12px">
        <button class="btn btn-primary" id="saveReview">Save Decision</button>
        ${review ? '<button class="btn btn-ghost" id="clearReviewBtn">Clear</button>' : ''}
      </div>
    </div>
  `;

  $('#detailBody').innerHTML = html;

  // Wire up review actions
  let selectedDecision = review ? review.decision : null;

  $$('#reviewActions .review-option').forEach(opt => {
    opt.addEventListener('click', () => {
      $$('#reviewActions .review-option').forEach(o => o.classList.remove('selected'));
      opt.classList.add('selected');
      selectedDecision = opt.dataset.decision;
    });
  });

  $('#saveReview').addEventListener('click', () => {
    if (!selectedDecision) return;
    const note = $('#reviewNote').value.trim();
    APP.reviews[demoId] = { decision: selectedDecision, note };
    record.reviewStatus = selectedDecision;
    closeDetail();
    updateQueueTable();
    renderTracking();
  });

  const clearBtn = $('#clearReviewBtn');
  if (clearBtn) {
    clearBtn.addEventListener('click', () => {
      delete APP.reviews[demoId];
      record.reviewStatus = 'pending';
      closeDetail();
      updateQueueTable();
      renderTracking();
    });
  }

  // Open panel
  $('#detailPanel').classList.add('open');
  $('#detailOverlay').classList.add('open');
};

function buildComparisonBar(label, value, mean, max, prefix = '') {
  const numVal = numeric(value);
  const numMean = numeric(mean);
  const numMax = numeric(max) > 0 ? Number(max) : 1;
  const pct = Math.min(100, Math.max(0, (numVal / numMax) * 100));
  const meanPct = Math.min(100, Math.max(0, (numMean / numMax) * 100));
  const isBelow = numVal < numMean;
  const color = isBelow ? 'var(--accent-amber)' : 'var(--accent-emerald)';
  const formattedMean = numMean === null ? 'Unavailable' : fmt(numMean, 1);
  return `
    <div class="comparison-bar-container">
      <div class="comparison-bar-label">
        <span style="color:var(--text-secondary)">${label}</span>
        <span style="color:var(--text-primary); font-weight:600">${prefix}${fmt(numVal)} <span style="color:var(--text-muted); font-weight:400">(test avg: ${prefix}${formattedMean})</span></span>
      </div>
      <div class="comparison-bar-track">
        <div class="comparison-bar-fill" style="width:${pct}%; background:${color}"></div>
        <div class="comparison-bar-marker" style="left:${meanPct}%" title="Test subset average: ${prefix}${formattedMean}"></div>
      </div>
    </div>`;
}

// ── Chart Rendering ───────────────────────────────────────────────────
function renderChart(canvasId, type, data, options = {}) {
  const canvas = document.getElementById(canvasId);
  if (!canvas) return;
  if (APP.charts[canvasId]) APP.charts[canvasId].destroy();

  const defaults = {
    responsive: true,
    maintainAspectRatio: true,
    plugins: {
      legend: { labels: { color: '#94a3b8' } },
    },
    animation: { duration: 600, easing: 'easeOutQuart' },
  };

  APP.charts[canvasId] = new Chart(canvas.getContext('2d'), {
    type,
    data,
    options: deepMerge(defaults, options),
  });
}

function deepMerge(target, source) {
  const result = { ...target };
  for (const key in source) {
    if (source[key] && typeof source[key] === 'object' && !Array.isArray(source[key])) {
      result[key] = deepMerge(result[key] || {}, source[key]);
    } else {
      result[key] = source[key];
    }
  }
  return result;
}

// ── Table Builder ─────────────────────────────────────────────────────
function buildTable(data, columns, labels, formatter) {
  let html = '<table class="data-table"><thead><tr>';
  columns.forEach(col => { html += `<th>${labels[col] || col}</th>`; });
  html += '</tr></thead><tbody>';
  data.forEach(row => {
    const display = formatter ? formatter(row) : row;
    html += '<tr>';
    columns.forEach(col => { html += `<td>${display[col] !== undefined ? display[col] : row[col] || ''}</td>`; });
    html += '</tr>';
  });
  html += '</tbody></table>';
  return html;
}

// ── Init ──────────────────────────────────────────────────────────────
document.addEventListener('DOMContentLoaded', initApp);