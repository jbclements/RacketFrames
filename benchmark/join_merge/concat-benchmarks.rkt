#lang typed/racket

#| class ConcatDataFrames(object):

    goal_time = 0.2
    params = ([0, 1], [True, False])
    param_names = ['axis', 'ignore_index']

    def setup(self, axis, ignore_index):
        frame_c = DataFrame(np.zeros((10000, 200),
                            dtype=np.float32, order='C'))
        self.frame_c = [frame_c] * 20
        frame_f = DataFrame(np.zeros((10000, 200),
                            dtype=np.float32, order='F'))
        self.frame_f = [frame_f] * 20

    def time_c_ordered(self, axis, ignore_index):
        concat(self.frame_c, axis=axis, ignore_index=ignore_index)

    def time_f_ordered(self, axis, ignore_index):
        concat(self.frame_f, axis=axis, ignore_index=ignore_index) |#
