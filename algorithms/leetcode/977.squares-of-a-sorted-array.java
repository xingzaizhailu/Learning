/*
 * @lc app=leetcode id=977 lang=java
 *
 * [977] Squares of a Sorted Array
 */

// @lc code=start
class Solution {
    public int[] sortedSquares(int[] A) {
        int[] ret = new int[A.length];
        int l = 0, r = A.length - 1, idx = A.length - 1;

        while (idx > -1) {
            if (Math.abs(A[l]) > Math.abs(A[r])) {
                ret[idx] = A[l] * A[l];
                ++l;
            } else {
                ret[idx] = A[r] * A[r];
                --r;
            }
            --idx;
        }
        return ret;
    }
}
// 8 mins
// @lc code=end

