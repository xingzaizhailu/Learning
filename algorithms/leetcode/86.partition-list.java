/*
 * @lc app=leetcode id=86 lang=java
 *
 * [86] Partition List
 */

// @lc code=start
/**
 * Definition for singly-linked list.
 * public class ListNode {
 *     int val;
 *     ListNode next;
 *     ListNode(int x) { val = x; }
 * }
 */
class Solution {
    public ListNode partition(ListNode head, int x) {
        ListNode fakeHead = new ListNode(x - 1);
        ListNode leTail = fakeHead, pre = fakeHead, cur = head;
        fakeHead.next = head;
        
        while (cur != null) {
            if (cur.val >= x) {
                pre = cur;
                cur = cur.next;
            } else if (cur == leTail.next) {
                cur = cur.next;
                pre = pre.next;
                leTail = leTail.next;
            } else {
                ListNode tmp = cur;
                cur = cur.next;
                pre.next = cur;
                
                tmp.next = leTail.next;
                leTail.next = tmp;
                leTail = leTail.next;
            }
        }
        
        return fakeHead.next;
    }
}
// @lc code=end

