/*
 * @lc app=leetcode id=81 lang=java
 *
 * [81] Search in Rotated Sorted Array II
 */

// @lc code=start
class Solution {
    public boolean search(int[] nums, int target) {
        if (nums.length == 0) return false;
        int lo = 0, hi = nums.length - 1, mid;
        
        while (lo < hi) {
            mid = (lo + hi) / 2;
            
            if (nums[mid] == target) return true;
            
            if (nums[lo] == nums[mid]) ++lo;
            else if (nums[lo] < nums[mid]) {
                if (target >= nums[lo] && target < nums[mid]) hi = mid - 1;
                else lo = mid + 1;
            } else { // lo > mid
                if (target > nums[mid] && target <= nums[hi]) lo = mid + 1;
                else hi = mid - 1;
            }
        }
        
        return nums[lo] == target ? true : false;
    }
}
// @lc code=end

