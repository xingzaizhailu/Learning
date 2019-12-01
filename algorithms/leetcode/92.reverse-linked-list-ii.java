/*
 * @lc app=leetcode id=92 lang=java
 *
 * [92] Reverse Linked List II
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
    public ListNode reverseBetween(ListNode head, int m, int n) {
        ListNode fakeHead = new ListNode(0);
        ListNode tail, cur = fakeHead, tmpHead, tmpTail;
        fakeHead.next = head;
        
        int count = -1;
        while (++count < m - 1) cur = cur.next;
        
        tail = cur;
        tmpHead = tmpTail = cur.next;
        cur = cur.next.next;
        ++count;
        
        while (++count <= n) {
            ListNode next = cur.next;
            cur.next = tmpHead;
            tmpHead = cur;
            cur = next;
        }
        
        tail.next = tmpHead;
        tmpTail.next = cur;
        
        return fakeHead.next;
    }
}
// @lc code=end

