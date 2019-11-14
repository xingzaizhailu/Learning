/*
 * @lc app=leetcode id=852 lang=java
 *
 * [852] Peak Index in a Mountain Array
 */

// @lc code=start
class Solution {
    public int peakIndexInMountainArray(int[] A) {
        if (A.length < 3) return -1;

        int low = 0, high = A.length - 1, mid;
        while (low <= high) {
            mid = (low + high) / 2;
            if (mid == 0 || mid == A.length - 1
                || A[mid] > A[mid + 1] && A[mid] > A[mid - 1]) {
                    return mid;
                }
            else if (A[mid] >= A[mid - 1]) {
                low = mid + 1;
            } else {
                high = mid - 1;
            }
        }
        return -1;
    }
}
// @lc code=end

