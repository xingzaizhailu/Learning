/*
 * @lc app=leetcode id=11 lang=java
 *
 * [11] Container With Most Water
 */

// @lc code=start
class Solution {
    public int maxArea(int[] height) {
        int left = 0, right = height.length - 1, max = 0, tmp;

        while (left < right) {
            tmp = Math.min(height[left], height[right]) * (right - left);
            max = tmp > max ? tmp : max;

            if (height[left] < height[right]) {
                ++left;
            } else {
                --right;
            }
        }

        return max;
    }
}
// 7mins
// @lc code=end

