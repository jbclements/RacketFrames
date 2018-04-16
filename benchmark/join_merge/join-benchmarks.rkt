#lang typed/racket

#| class Join(object):

    goal_time = 0.2
    params = [True, False]
    param_names = ['sort']

    def setup(self, sort):
        level1 = tm.makeStringIndex(10).values
        level2 = tm.makeStringIndex(1000).values
        label1 = np.arange(10).repeat(1000)
        label2 = np.tile(np.arange(1000), 10)
        index2 = MultiIndex(levels=[level1, level2],
                            labels=[label1, label2])
        self.df_multi = DataFrame(np.random.randn(len(index2), 4),
                                  index=index2,
                                  columns=['A', 'B', 'C', 'D'])

        self.key1 = np.tile(level1.take(label1), 10)
        self.key2 = np.tile(level2.take(label2), 10)
        self.df = DataFrame({'data1': np.random.randn(100000),
                             'data2': np.random.randn(100000),
                             'key1': self.key1,
                             'key2': self.key2})

        self.df_key1 = DataFrame(np.random.randn(len(level1), 4),
                                 index=level1,
                                 columns=['A', 'B', 'C', 'D'])
        self.df_key2 = DataFrame(np.random.randn(len(level2), 4),
                                 index=level2,
                                 columns=['A', 'B', 'C', 'D'])

        shuf = np.arange(100000)
        np.random.shuffle(shuf)
        self.df_shuf = self.df.reindex(self.df.index[shuf])

    def time_join_dataframe_index_multi(self, sort):
        self.df.join(self.df_multi, on=['key1', 'key2'], sort=sort)

    def time_join_dataframe_index_single_key_bigger(self, sort):
        self.df.join(self.df_key2, on='key2', sort=sort)

    def time_join_dataframe_index_single_key_small(self, sort):
        self.df.join(self.df_key1, on='key1', sort=sort)

    def time_join_dataframe_index_shuffle_key_bigger_sort(self, sort):
        self.df_shuf.join(self.df_key2, on='key2', sort=sort) |#
