using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[ExecuteInEditMode]
public class Mask : MonoBehaviour
{
	public Transform Volume = null;
	public Color VolumeGizmoColor = Color.white;

	[Space]
	public Material Material = null;

	private float _radius = 0.0f;

	private void OnDrawGizmos()
	{
		if (Volume == null || Material == null)
		{ return; }

		Gizmos.color = VolumeGizmoColor;
		Gizmos.DrawWireSphere(Volume.position, _radius);
	}

	private void Update()
	{
		if (Volume == null || Material == null)
		{ return; }

		// Better to use Material Property Blocks and Instancing
		Material.SetVector("_Center", Volume.position);

		_radius = Mathf.Max(Mathf.Max(Volume.lossyScale.x, Volume.lossyScale.y), Volume.lossyScale.z) * 0.5f;
		Material.SetFloat("_Radius", _radius);
	}
}
