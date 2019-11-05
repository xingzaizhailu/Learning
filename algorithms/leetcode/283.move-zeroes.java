/*
 * @lc app=leetcode id=283 lang=java
 *
 * [283] Move Zeroes
 */

// @lc code=start
class Solution {
    public void moveZeroes(int[] nums) {
        for (int i = nums.length - 2 ; i > -1; --i) {
            if (nums[i] != 0) continue;

            for (int j = i; j < nums.length; ++j) {
                if (j + 1 != 0 && j + 1 < nums.length) nums[j] = nums[j+1];
                else nums[j] = 0;
            }
        }
    }
}
// @lc code=end

