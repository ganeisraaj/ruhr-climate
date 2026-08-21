import streamlit as st
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import ks_2samp, ttest_ind

@st.cache_data
def load_all_stations():
    stations_meta = {
        "Kahler Asten": {"file": "streamlit/indexTG_000812.txt", "elevation": 839},
        "Dortmund":     {"file": "streamlit/indexTG_004021.txt", "elevation": 120},
        "Duisburg":     {"file": "streamlit/indexTG_004030.txt", "elevation": 31},
        "Essen":        {"file": "streamlit/indexTG_004074.txt", "elevation": 150},
        "Arnsberg":     {"file": "streamlit/indexTG_004172.txt", "elevation": 218},
        "Brilon":       {"file": "streamlit/indexTG_004897.txt", "elevation": 472},
    }

    def load_station(filepath, elevation):
        df = pd.read_csv(
            filepath,
            skiprows=30,
            header=None,
            sep=r'\s+',
            names=[
                "souid", "year", "annual", "winter_half", "summer_half",
                "djf", "mam", "jja", "son",
                "jan", "feb", "mar", "apr", "may", "jun",
                "jul", "aug", "sep", "oct", "nov", "dec"
            ]
        )
        df = df.replace(-999999, np.nan)
        temp_cols = df.columns.drop(["souid", "year"])
        df[temp_cols] = df[temp_cols] / 100
        df["elevation"] = elevation
        df = df[df["year"] < 2026]
        return df

    dfs = {}
    for name, meta in stations_meta.items():
        dfs[name] = load_station(meta["file"], meta["elevation"])
    return dfs

dfs = load_all_stations()

st.title("Ruhr Climate Analysis")
st.markdown(
    "Compare temperature distributions across time periods and stations "
    "using formal statistical tests. Based on ECA&D data from six NRW weather stations."
)

st.sidebar.header("Settings")

station = st.sidebar.selectbox(
    "Station",
    ["Kahler Asten", "Duisburg", "Essen", "Dortmund", "Arnsberg", "Brilon"]
)

season = st.sidebar.selectbox(
    "Season",
    ["annual", "djf", "jja"],
    format_func=lambda x: {"annual": "Annual", "djf": "Winter (DJF)", "jja": "Summer (JJA)"}[x]
)

df = dfs[station]
year_min = int(df["year"].min())
year_max = int(df["year"].max())

st.sidebar.markdown("**Period 1**")
p1_start = st.sidebar.slider("Start", year_min, year_max - 10, max(year_min, 1931), key="p1s")
p1_end = st.sidebar.slider("End", year_min, year_max, min(year_max, 1960), key="p1e")

st.sidebar.markdown("**Period 2**")
p2_start = st.sidebar.slider("Start", year_min, year_max - 10, max(year_min, 1991), key="p2s")
p2_end = st.sidebar.slider("End", year_min, year_max, min(year_max, 2020), key="p2e")

p1 = df[(df["year"] >= p1_start) & (df["year"] <= p1_end)][season].dropna()
p2 = df[(df["year"] >= p2_start) & (df["year"] <= p2_end)][season].dropna()

st.header("Descriptive Statistics")

col1, col2 = st.columns(2)
with col1:
    st.metric(f"Period 1 mean ({p1_start}-{p1_end})", f"{p1.mean():.2f}°C")
    st.metric("n", len(p1))
with col2:
    st.metric(f"Period 2 mean ({p2_start}-{p2_end})", f"{p2.mean():.2f}°C")
    st.metric("n", len(p2))

st.metric("Raw difference", f"{p2.mean() - p1.mean():.2f}°C")

st.header("Hypothesis Tests")

if len(p1) < 3 or len(p2) < 3:
    st.warning("Not enough data in one or both periods. Adjust the sliders.")
else:
    t_stat, p_t = ttest_ind(p1, p2, equal_var=False)
    d_stat, p_ks = ks_2samp(p1, p2)
    pooled_sd = np.sqrt(
        ((len(p1)-1)*p1.std()**2 + (len(p2)-1)*p2.std()**2) / (len(p1)+len(p2)-2)
    )
    cohens_d = (p2.mean() - p1.mean()) / pooled_sd

    col1, col2 = st.columns(2)
    with col1:
        st.subheader("Welch t-test")
        st.write(f"t = {t_stat:.3f}")
        st.write(f"p = {p_t:.4f}")
        if p_t < 0.05:
            st.success("Significant at α = 0.05")
        else:
            st.warning("Not significant at α = 0.05")

    with col2:
        st.subheader("KS Test")
        st.write(f"D = {d_stat:.3f}")
        st.write(f"p = {p_ks:.4f}")
        if p_ks < 0.05:
            st.success("Significant at α = 0.05")
        else:
            st.warning("Not significant at α = 0.05")

    st.metric("Cohen's d", f"{cohens_d:.3f}")

st.header("Temperature Over Time")

fig, ax = plt.subplots(figsize=(10, 4))
df_clean = df.dropna(subset=[season])
ax.plot(df_clean["year"], df_clean[season], color="steelblue", alpha=0.8)
ax.axvspan(p1_start, p1_end, alpha=0.2, color="red", label="Period 1")
ax.axvspan(p2_start, p2_end, alpha=0.2, color="blue", label="Period 2")
ax.set_xlabel("Year")
ax.set_ylabel("Temperature (°C)")
ax.legend()
plt.tight_layout()
st.pyplot(fig)
