using UnityEngine;

[ExecuteInEditMode]
public class FreeCamera : MonoBehaviour
{
    [Tooltip("Regular speed.")] public float speed = 100.0f;

    [Tooltip("Multiplied by how long shift is held.")]
    public float shiftSpeed = 250.0f;

    [Tooltip("Maximum speed when holdin shift.")]
    public float shiftMaxSpeed = 1000.0f;

    [Tooltip("Mouse sensivity.")] public float sensivity = 0.25f;

    private Vector3
        _lastMouse = new Vector3(255, 255, 255); //kind of in the middle of the screen, rather than at the top (play)

    private float _totalRun = 1.0f;
    private bool _enableMove = false;

    private void Start()
    {
        _lastMouse = Input.mousePosition;
    }

    private void Update()
    {
        _enableMove = false;

        //Mouse  camera angle
        if (Input.GetMouseButtonDown(1))
        {
            _lastMouse = Input.mousePosition;
        }

        if (Input.GetMouseButton(1))
        {
            _lastMouse = Input.mousePosition - _lastMouse;
            _lastMouse = new Vector3(-_lastMouse.y * sensivity, _lastMouse.x * sensivity, 0);
            _lastMouse = new Vector3(transform.eulerAngles.x + _lastMouse.x, transform.eulerAngles.y + _lastMouse.y, 0);
            transform.eulerAngles = _lastMouse;
            _lastMouse = Input.mousePosition;
            _enableMove = true;
        }
        else
        {
            _lastMouse = Input.mousePosition;
        }

        //Keyboard commands
        if (!_enableMove)
        {
            return;
        }

        float f = 0.0f;
        var p = GetBaseInput();
        if (Input.GetKey(KeyCode.LeftShift))
        {
            _totalRun += Time.deltaTime;
            p = p * _totalRun * shiftSpeed;
            p.x = Mathf.Clamp(p.x, -shiftMaxSpeed, shiftMaxSpeed);
            p.y = Mathf.Clamp(p.y, -shiftMaxSpeed, shiftMaxSpeed);
            p.z = Mathf.Clamp(p.z, -shiftMaxSpeed, shiftMaxSpeed);
        }
        else
        {
            _totalRun = Mathf.Clamp(_totalRun * 0.5f, 1, 1000);
            p = p * speed;
        }

        p = p * Time.deltaTime;
        if (Input.GetKey(KeyCode.Space))
        {
            //If player wants to move on X and Z axis only
            f = transform.position.y;
            transform.Translate(p);
            transform.position.Set(transform.position.x, f, transform.position.z);
        }
        else
        {
            transform.Translate(p);
        }
    }

    private Vector3 GetBaseInput()
    {
        Vector3 velocity = Vector3.zero;
        if (Input.GetKey(KeyCode.E))
        {
            velocity += new Vector3(0, 1, 0);
        }

        if (Input.GetKey(KeyCode.Q))
        {
            velocity += new Vector3(0, -1, 0);
        }

        if (Input.GetKey(KeyCode.W))
        {
            velocity += new Vector3(0, 0, 1);
        }

        if (Input.GetKey(KeyCode.S))
        {
            velocity += new Vector3(0, 0, -1);
        }

        if (Input.GetKey(KeyCode.A))
        {
            velocity += new Vector3(-1, 0, 0);
        }

        if (Input.GetKey(KeyCode.D))
        {
            velocity += new Vector3(1, 0, 0);
        }

        return velocity;
    }
}