def under_sample_by_label(split_trainable, model_type, conf, logger):
    """
    Under sample the trainable data set by label. Reduce the 0s in a binary classification problem
    such that the ratio of 0s to 1s in the data set is sampling_ratio:1

    Original: AIFunctions.undersample_df

    """
    if not conf.is_under_sample_enabled:
        return split_trainable

    label = conf.event_label(model_type)
    count_by_label = {
        row[label]: row["count"]
        for row in split_trainable.groupBy(label).count().collect()
    }

    under_sample_ratio_max = count_by_label[0] / count_by_label[1]

    negative_share = 1.0
    if conf.under_sample_ratio < under_sample_ratio_max:
        negative_share = conf.under_sample_ratio * count_by_label[1] / count_by_label[0]
    else:
        logger.warn(
            "Using the max possible under_sample_ratio: {0}, instead of the specified {1}".format(
                under_sample_ratio_max, conf.under_sample_ratio
            )
        )

    fractions = {1: 1.0, 0: negative_share}

    # Log important info
    info = common.info_node(conf.info, "under_sample_by_label")
    info["negative_share"] = negative_share
    info["under_sample_ratio_max"] = under_sample_ratio_max

    return data_frame_util.stratified_sample(
        label, split_trainable, fractions, conf.randomization_seed
