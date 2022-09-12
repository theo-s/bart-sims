import os

import numpy as np
import pandas as pd

import matplotlib

matplotlib.use('TkAgg')
import matplotlib.pyplot as plt
TEXT_SIZE = 36
BART_VERSION = "trees_1_restricted"
matplotlib.rc('xtick', labelsize=TEXT_SIZE)
matplotlib.rc('ytick', labelsize=TEXT_SIZE)
matplotlib.rcParams.update({'font.size': TEXT_SIZE})


def get_ndp(ndp, ds_name):
    if np.isfinite(ndp):
        return ndp
    elif ds_name == "echo_months":
        return 15746
    elif ds_name == "satellite_image":
        return 5791
    elif ds_name == "california_housing":
        return 18576
    elif ds_name == "breast_tumor":
        return 104976


def _get_split_file(ds_name, run, ndp):
    f_name = f"burn_5000_post_1000_nchain_8_run_{run}_ndp_{ndp}_first_split.csv"
    file_path = os.path.join("data", "aistats", BART_VERSION, ds_name, f_name)
    return file_path


def _read_split_data(ds_name, run, ndp):
    f_name = _get_split_file(ds_name, run, ndp)
    return pd.read_csv(f_name).iloc[:, 1:]

def _get_plt_ds_name(ds_name):
    return ds_name.replace("_", " ").capitalize()

def plot_chains_heatmap(ds_names, runs):
    fig, axis = plt.subplots(nrows=len(ds_names), ncols=2, figsize=(25, 25))
    fig.tight_layout(pad=3.0)  # Or equivalently,  "plt.tight_layout()"

    for i, ds_name in enumerate(ds_names):
        run = runs[i]
        df_large = _read_split_data(ds_name, run, get_ndp(np.inf, ds_name))
        heatmap_data_large = pd.crosstab(df_large['chain'], df_large['var']).iloc[:, 1:]

        yticks_labels = np.arange(1, 9)
        yticks_loc = yticks_labels - 0.5

        # xticks_labels = np.arange(1, heatmap_data_large.shape[0]+1)
        # xticks_loc = xticks_labels - 0.5

        l_ax = axis[i, 1]
        l_ax.set_yticks(yticks_loc)
        l_ax.set_yticklabels(yticks_labels)
        l_ax.set_xticks([])
        # l_ax.set_xticklabels(xticks_labels)

        im = l_ax.pcolor(heatmap_data_large, cmap=matplotlib.cm.Blues, vmin=0, vmax=6000)
        l_ax.set_xlabel("Index of Root Split Feature", fontsize=TEXT_SIZE)
        l_ax.set_title(f"Dataset: {_get_plt_ds_name(ds_name)}\nn: {get_ndp(np.inf, ds_name)}", loc="left")

        df_small = _read_split_data(ds_name, run, get_ndp(200, ds_name))
        heatmap_data_small = pd.crosstab(df_small['chain'], df_small['var']).iloc[:, 1:]
        # xticks_labels = np.arange(1, heatmap_data_small.shape[0]+1)
        # xticks_loc = xticks_labels - 0.5
        s_ax = axis[i, 0]
        s_ax.set_xlabel("Index of Root Split Feature", fontsize=TEXT_SIZE)
        s_ax.set_ylabel("Chain", fontsize=TEXT_SIZE)
        s_ax.set_yticks(yticks_loc)
        s_ax.set_yticklabels(yticks_labels)
        s_ax.set_xticks([])
        # s_ax.set_xticklabels(xticks_labels)
        s_ax.set_title(f"Dataset: {_get_plt_ds_name(ds_name)}\nn: {200}", loc="left")

        s_ax.pcolor(heatmap_data_small, cmap=matplotlib.cm.Blues, vmin=0, vmax=6000)
    fig.subplots_adjust(right=1.0)
    # cbar_ax = fig.add_axes([0.85, 0.2, 0.05, 0.7])
    cbar = fig.colorbar(im, ax=axis.ravel().tolist())
    cbar.ax.get_yaxis().labelpad = 40

    cbar.set_label('Number of Samples', rotation=270, fontsize=TEXT_SIZE)

    plt.savefig(os.path.join("figures", "aistats", BART_VERSION, f"root_var_{BART_VERSION}.png"), dpi=300)
    plt.close()

def main():
    plot_chains_heatmap(['breast_tumor', "california_housing"], runs=[2,3])


if __name__ == '__main__':
    main()
