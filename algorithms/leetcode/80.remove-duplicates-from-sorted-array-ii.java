/*
 * @lc app=leetcode id=80 lang=java
 *
 * [80] Remove Duplicates from Sorted Array II
 */

// @lc code=start
class Solution {
    public int removeDuplicates(int[] nums) {
        if (nums.length == 0) return 0;
        int i = 0, cur = 1, last = nums[0], freq = 1;
        
        for (i = 1; i < nums.length; ++i) {
            if (nums[i] == last) {
                ++freq;
                if (freq <= 2) {
                    nums[cur++] = nums[i];
                }
            } else {
                nums[cur++] = nums[i];
                last = nums[i];
                freq = 1;
            }
        }
        
        return cur;
    }
}
// @lc code=end

