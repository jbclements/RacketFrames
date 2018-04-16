#lang typed/racket

#| class Append(object):

    goal_time = 0.2

    def setup(self):
        self.df1 = DataFrame(np.random.randn(10000, 4),
                             columns=['A', 'B', 'C', 'D'])
        self.df2 = self.df1.copy()
        self.df2.index = np.arange(10000, 20000)
        self.mdf1 = self.df1.copy()
        self.mdf1['obj1'] = 'bar'
        self.mdf1['obj2'] = 'bar'
        self.mdf1['int1'] = 5
        try:
            with warnings.catch_warnings(record=True):
                self.mdf1.consolidate(inplace=True)
        except:
            pass
        self.mdf2 = self.mdf1.copy()
        self.mdf2.index = self.df2.index

    def time_append_homogenous(self):
        self.df1.append(self.df2)

    def time_append_mixed(self):
        self.mdf1.append(self.mdf2)
 |#