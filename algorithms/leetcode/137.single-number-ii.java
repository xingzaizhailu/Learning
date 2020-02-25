/*
 * @lc app=leetcode id=137 lang=java
 *
 * [137] Single Number II
 */

// @lc code=start
class Solution {
    public int singleNumber(int[] nums) {
        int single = nums[0];
        
        for (int i = 1; i < nums.length; ++i) {
            if (nums[i] == single) single = nums[++i];
        }
        
        return single;
    }
}
// @lc code=end

