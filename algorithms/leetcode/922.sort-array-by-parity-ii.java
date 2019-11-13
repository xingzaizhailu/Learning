/*
 * @lc app=leetcode id=922 lang=java
 *
 * [922] Sort Array By Parity II
 */

// @lc code=start
class Solution {
    public int[] sortArrayByParityII(int[] A) {
        for (int i = 0; i < A.length; ++i) {
            if (A[i] % 2 == i % 2) continue;

            int tmp;
            for (int j = i + 1; j < A.length; ++j) {
                if (A[j] % 2 == i % 2) {
                    tmp = A[i];
                    A[i] = A[j];
                    A[j] = tmp;
                    break;
                }
            }
        }

        return A;
    }
}
// 15 mins
// @lc code=end

