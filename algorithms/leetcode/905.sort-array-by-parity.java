/*
 * @lc app=leetcode id=905 lang=java
 *
 * [905] Sort Array By Parity
 */

// @lc code=start
class Solution {
    public int[] sortArrayByParity(int[] A) {
        int l = 0, r = A.length - 1, tmp;

        while (l < r) {
            while (l < r && A[l] % 2 == 0) ++l;
            while (l < r && A[r] % 2 != 0) --r;

            tmp = A[l];
            A[l] = A[r];
            A[r] = tmp;
        }

        return A;
    }
}
// 4 mins
// @lc code=end

