/*
 * @lc app=leetcode id=153 lang=java
 *
 * [153] Find Minimum in Rotated Sorted Array
 */

// @lc code=start
class Solution {
    public int findMin(int[] nums) {
        int low = 0, high = nums.length - 1, mid, ret = -1;
        
        while (low < high) {
            mid = (low + high) / 2;
            if (mid == 0 && nums[mid] < nums[mid+1]) {
                ret = nums[mid];
                break;
            } else if (mid > 0 && nums[mid] < nums[mid-1]) {
                ret = nums[mid];
                break;
            }
            
            if (nums[mid] > nums[high]) {
                low = mid + 1;
            } else {
                high = mid - 1;
            }
        }
        if (ret == -1) ret = nums[low];
        return ret;
    }
}
// @lc code=end

