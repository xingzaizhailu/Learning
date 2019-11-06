/*
 * @lc app=leetcode id=189 lang=java
 *
 * [189] Rotate Array
 */

// @lc code=start
class Solution {
    public void rotate(int[] nums, int k) {
        if (k < 0) return;

        k %= nums.length; // avoid redundant shifts
        int numShift = 0; // nums.length shifts expected as the end

        // move the ith elem 'cur' to its destination directly
        for (int i = 0; i < k; ++i) {
            int cur = nums[i], next, nextIdx = (i + k) % nums.length;

            /* for moving 'cur', need to temporarily save next elem to 'next'
             * and also move 'next' to its destination right after
             * and next elem of 'next' as well, etc...
             */
            while (nextIdx != i && numShift < nums.length) {
                next = nums[nextIdx];
                nums[nextIdx] = cur;
                cur = next;

                nextIdx = (nextIdx + k) % nums.length;
                ++numShift;
            }

            if (numShift < nums.length) {
                nums[nextIdx] = cur;
                ++numShift;
            } else {
                break;
            }
        }
    }
}
// @lc code=end

